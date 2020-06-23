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
#' @param metric A `character` vector specifying the h2o metric to use as the
#'   scoring criterion for the hyperparameter tuning. Must be one of  c("rsq",
#'   "sensitivity", "rmse", "accuracy", "mn_log_loss", "mse"). The default is
#'   "rsq" for regression models and "mn_log_loss" for classification models.
#' @param verbose Whether to provide progress messages.
#'
#' @return
#' @export
tune_grid_h2o <-
  function(object,
           preprocessor = NULL,
           resamples,
           param_info = NULL,
           grid = 10,
           metric = NULL,
           verbose = FALSE) {

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
    mode <- object$mode
    model_args <- object$args

    # process scoring metric
    if (is.null(metric) & mode == "classification")
      metric <- "mn_log_loss"
    if (is.null(metric) & mode == "regression")
      metric <- "rsq"

    yardstick_metric <- convert_h2o_metrics(metric)

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
    resamples$.metrics <-
      purrr::map2(assessment_indices, seq_along(assessment_indices), function(ids, n) {
        if (verbose)
          message(glue::glue("Fitting resample {n}"))

        grid_args <- list(
          grid_id = paste(algorithm, "grid", as.integer(stats::runif(1, 0, 1e9)), sep = "_"),
          algorithm = algorithm,
          x = predictors,
          y = outcome,
          training_frame = full_data_h2o[-ids, ],
          validation_frame = full_data_h2o[ids, ],
          hyper_params = params
        )

        res <- make_h2o_call("h2o.grid", grid_args, model_args)

        scores <- h2o::h2o.getGrid(
          grid_id = grid_args$grid_id,
          sort_by = yardstick_metric$name,
          decreasing = FALSE
        )
        scores <- as.data.frame(scores@summary_table)
        scores[, ncol(scores)] <- as.numeric(scores[, ncol(scores)])

        for (x in names(params)) {
          scores[[x]] <- gsub("\\[", "", scores[[x]])
          scores[[x]] <- gsub("\\]", "", scores[[x]])
        }

        scores %>%
          as_tibble() %>%
          dplyr::select(-dplyr::one_of("model_ids")) %>%
          dplyr::mutate_at(tuning_args, as.numeric) %>%
          dplyr::mutate(
            .metric = metric,
            .estimator = dplyr::if_else(mode == "classification", "multiclass", "standard")
          ) %>%
          dplyr::rename(.estimate = !!yardstick_metric$name) %>%
          dplyr::rename(rename_tuning_args)
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
    attr(resamples, "metrics") <- yardstick_metric$metric_set

    resamples
  }
