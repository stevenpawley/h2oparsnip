#' Tune h2o models
#'
#' This is a prototype of a version of tune_grid that uses h2o.grid to perform
#' hyperparameter tuning.
#'
#' @section Limitations:
#' - Only model arguments can be tuned, not arguments in the preprocessing recipes.
#'
#' - Parsnip only allows `data.frame` and `tbl_spark` objects to be passed
#' to the `fit` method, not `H2OFrame` objects.
#'
#' @param object A parsnip `model_spec` object.
#' @param preprocessor A `recipe` object.
#' @param resamples An `rset` object.
#' @param param_info A `dials::parameters()` object or NULL. If none is given, a
#'   parameters set is derived from other arguments. Passing this argument can
#'   be useful when parameter ranges need to be customized.
#' @param grid A `data.frame` of tuning combinations or a positive integer. The
#'   data frame should have columns for each parameter being tuned and rows for
#'   tuning parameter candidates. An integer denotes the number of candidate
#'   parameter sets to be created automatically. If a positive integer is used
#'   or no tuning grid is supplied, then a semi-random grid via
#'   `dials::grid_latin_hypercube` is created based on the specified number of
#'   tuning iterations (default size = 10).
#' @param metrics A `yardstick::metric_set` or NULL. Note that not all yardstick
#'   metrics can be used with `tune_grid_h2o`. The metrics must be one of
#'   `yardstick::rsq`, `yardstick::sensitivity`, `yardstick::rmse`,
#'   `yardstick::accuracy`, `yardstick::mn_log_loss`, or `h2oparsnip::mse`. If
#'   NULL then the default is `yardstick::rsq` for regression models and
#'   `yardstick::mn_log_loss` for classification models.
#' @param ... Not currently used.
#'
#' @return
#' @export
tune_grid_h2o <-
  function(object,
           preprocessor = NULL,
           resamples,
           param_info = NULL,
           grid = 10,
           metrics = NULL,
           ...) {

    # convert workflow to model_spec and recipe objects
    if (inherits(object, "workflow")) {
      preprocessor <- workflows::pull_workflow_preprocessor(object)
      object <- workflows::pull_workflow_spec(object)
    }

    # tuning grid
    if (is.null(param_info)) {
      param_info <- tune::parameters(object)
    }

    if (inherits(grid, "numeric")) {
      grid <- dials::grid_latin_hypercube(param_info, size = grid)
    }

    # get model mode
    model_mode <- object$mode
    model_args <- object$args

    # process scoring metric
    if (is.null(metrics) & model_mode == "classification")
      metrics <- metric_set(yardstick::mn_log_loss)
    if (is.null(metrics) & model_mode == "regression")
      metrics <- metric_set(yardstick::rsq)

    h2o_metrics <- convert_h2o_metrics(metrics)

    # extract complete dataset from resamples
    data_train <- rsample::training(resamples$splits[[1]])
    data_test <- rsample::testing(resamples$splits[[1]])
    full_data <- dplyr::bind_rows(data_train, data_test)
    row_order <-
      c(as.numeric(rownames(data_train)), as.numeric(rownames(data_test)))
    full_data <- as_tibble(full_data[order(row_order), ])

    # prep the recipe
    prepped_recipe <- preprocessor %>% recipes::prep()
    full_data <- prepped_recipe %>% recipes::bake(full_data)

    # get predictor and outcome terms
    outcome <- prepped_recipe$term_info %>%
      dplyr::filter(!!rlang::sym("role") == "outcome") %>%
      dplyr::pull(!!rlang::sym("variable"))

    predictors <- prepped_recipe$term_info %>%
      dplyr::filter(!!rlang::sym("role") == "predictor") %>%
      dplyr::pull(!!rlang::sym("variable"))

    form <- paste(outcome, "~", paste(predictors, collapse = " + "))
    form <- stats::as.formula(form)

    # replace descriptors
    if (parsnip:::requires_descrs(object)) {
      data_stats <- parsnip:::get_descr_form(form, full_data)
      parsnip:::scoped_descrs(data_stats)
    }

    # get row indexes of assessment set data for each rsplit
    assessment_indices <-
      purrr::map(resamples$splits, rsample::complement)

    # pass full data to h2o
    full_data_h2o <-
      h2o::as.h2o(full_data, destination_frame = "grid_data")

    # translate parsnip arguments to h2o
    alg <- model_spec_to_algorithm(object, model_args)
    algorithm <- alg[[1]]
    model_name <- alg[[2]]
    model_args <- alg[[3]]

    original_names <- translate_args(model_name) %>%
      dplyr::select(dplyr::all_of(c("parsnip", "h2o"))) %>%
      tidyr::drop_na()

    nm <- original_names$h2o %>%
      rlang::set_names(original_names$parsnip)

    model_args <- rename_list(model_args, nm)
    tuning_args <- nm[tune::tune_args(object)$name]
    tuning_args <- as.character(tuning_args)
    rename_tuning_args <- nm[nm %in% tuning_args]

    # convert grid to list
    params <- as.list(grid)
    params <- purrr::map(params, ~ .x[!duplicated(.x)])
    params <- rename_list(params, nm)

    # get model args
    null_args <-
      sapply(model_args, function(x)
        is.null(rlang::eval_tidy(x)))
    model_args <- model_args[!null_args]

    # check tunable
    if (!any(tuning_args %in% names(params))) {
      missing_tune <- tuning_args[!tuning_args %in% names(params)]
      rlang::abort(paste("missing arguments", missing_tune))
    }

    model_args <- model_args[!names(model_args) %in% tuning_args]
    model_args <- append(model_args, object$eng_args)

    if (length(model_args) == 0)
      model_args <- NULL

    # fit h2o.grid on each resample
    resamples$.metrics <- purrr::map(assessment_indices, function(ids) {
      grid_args <- list(
        grid_id = generate_random_id(glue::glue("{algorithm}_grid")),
        algorithm = algorithm,
        x = predictors,
        y = outcome,
        training_frame = full_data_h2o[-ids,],
        validation_frame = full_data_h2o[ids,],
        hyper_params = params
      )

      res <- make_h2o_call("h2o.grid", grid_args, model_args)

      scores_df <-
        purrr::map2_dfr(
          h2o_metrics,
          names(h2o_metrics),
          extract_h2o_scores,
          grid_args$grid_id,
          tuning_args,
          params,
          rename_tuning_args,
          model_mode
        )

      return(scores_df)
    })

    class(resamples) <- c("tune_results", class(resamples))

    arg_names <- names(grid)
    param_list <- map(
      arg_names,
      ~ glue::glue("dials::{.x}()") %>%
        rlang::parse_expr() %>%
        rlang::eval_tidy()
    )
    attr(resamples, "parameters") <- dials::parameters(param_list)

    names(attributes(metrics)$metrics) <-
      gsub("yardstick::", "", names(attributes(metrics)$metrics))
    names(attributes(metrics)$metrics) <-
      gsub("h2oparsnip::", "", names(attributes(metrics)$metrics))
    attr(resamples, "metrics") <- metrics

    resamples
  }


extract_h2o_scores <-
  function(h2o_metric_name,
           yardstick_metric_name,
           grid_id,
           tuning_args,
           params,
           rename_tuning_args,
           model_mode) {
    scores <- h2o::h2o.getGrid(grid_id = grid_id,
                               sort_by = h2o_metric_name,
                               decreasing = FALSE)

    scores <- as.data.frame(scores@summary_table)
    scores[, ncol(scores)] <-
      as.numeric(scores[, ncol(scores)])

    for (x in names(params)) {
      scores[[x]] <- gsub("\\[", "", scores[[x]])
      scores[[x]] <- gsub("\\]", "", scores[[x]])
    }

    scores <- scores %>%
      as_tibble() %>%
      dplyr::select(-dplyr::one_of("model_ids")) %>%
      dplyr::mutate_at(tuning_args, as.numeric) %>%
      dplyr::mutate(
        .metric = yardstick_metric_name,
        .estimator = dplyr::if_else(model_mode == "classification", "multiclass", "standard")
      ) %>%
      dplyr::rename(.estimate = !!h2o_metric_name) %>%
      dplyr::rename(dplyr::all_of(rename_tuning_args))

    return(scores)
  }


model_spec_to_algorithm <- function(object, model_args) {

  if (inherits(object, "boost_tree")) {
    model_name <- "boost_tree"
    algorithm <- "gbm"
  }
  if (inherits(object, "rand_forest")) {
    model_name <- "rand_forest"
    algorithm <- "randomForest"
  }
  if (inherits(object, "linear_reg")) {
    model_name <- "linear_reg"
    algorithm <- "glm"
    model_args$family = "gaussian"
  }
  if (inherits(object, "logistic_reg")) {
    model_name <- "logistic_reg"
    algorithm <- "glm"
    model_args$family = "binomial"
  }
  if (inherits(object, "multinom_reg")) {
    model_name <- "multinom_reg"
    algorithm <- "glm"
    model_args$family = "multinomial"
  }
  if (inherits(object, "mlp")) {
    model_name <- "mlp"
    algorithm <- "deeplearning"
  }
  if (inherits(object, "naive_Bayes")) {
    model_name <- "naive_Bayes"
    algorithm <- "naiveBayes"
  }

  return(list(algorithm, model_name, model_args))
}
