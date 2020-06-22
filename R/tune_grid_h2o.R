# library(tidymodels)
# library(h2oparsnip)
# library(h2o)
# library(yardstick)
# h2o.init()
#
# object <-
#   parsnip::multinom_reg(mode = "classification", penalty = tune(), mixture = tune()) %>%
#   set_engine("h2o", seed = 1234)
# rec_obj <- iris %>% recipe(Species ~.)
# object <- workflow() %>%
#   add_recipe(rec_obj) %>%
#   add_model(object)
# grid <- expand.grid(penalty = c(0.1, 1), mixture = c(0.1, 0.3, 0.5, 0.7, 1.0))
#
# resamples <- vfold_cv(iris, v = 2)
# res <- tune_grid_h2o(object, resamples = resamples, grid = grid)
# collect_metrics(res)
# attributes(res)
# select_best(res, metric = "mn_log_loss")


#' Tune h2o models
#'
#' This is a prototype of a version of tune_grid that uses h2o.grid to perform
#' hyperparameter tuning.
#'
#' Current limitations
#' -------------------
#' - Only model arguments can be tuned
#' - Parsnip descriptors are not supported
#' - Custom metrics are not support. Currently r2 is used for regression and
#' logloss is used for classification.
#' - Parsnip only allows `data.frame` and `tbl_spark` objects to be passed
#' to the `fit` method, not `H2OFrame` objects.
#'
#' @param object A parsnip model spec object.
#' @param preprocessor A recipe object.
#' @param resamples A rset object.
#' @param grid A tibble of hyperparameter values.
#' @param verbose Whether to provide progress messages.
#'
#' @return
#' @export
#' @importFrom yardstick mn_log_loss rsq
tune_grid_h2o <-
  function(object,
           preprocessor = NULL,
           resamples,
           grid,
           verbose = FALSE) {

  # tuning model params only ----
  if (inherits(object, "workflow")) {
    preprocessor <- workflows::pull_workflow_preprocessor(object)
    object <- workflows::pull_workflow_spec(object)
  }

  mode <- object$mode
  model_args <- object$args

  if (mode == "classification") {
    metric <- "logloss"
    yardstick_name <- "mn_log_loss"
  } else {
    metric <- "r2"
    yardstick_name <- "rsq"
  }

  # get complete dataset from resamples
  data_train <- rsample::training(resamples$splits[[1]])
  data_test <- rsample::testing(resamples$splits[[1]])
  full_data <- dplyr::bind_rows(data_train, data_test)

  row_order <- c(as.numeric(rownames(data_train)), as.numeric(rownames(data_test)))
  full_data <- as_tibble(full_data[order(row_order), ])

  prepped_recipe <- preprocessor %>% recipes::prep()
  full_data <- prepped_recipe %>% recipes::bake(full_data)

  # get terms
  outcome <- prepped_recipe$term_info %>%
    dplyr::filter(role == "outcome") %>%
    dplyr::pull(variable)

  predictors <- prepped_recipe$term_info %>%
    dplyr::filter(role == "predictor") %>%
    dplyr::pull(variable)

  # get row indexes of assessment set data for each rsplit
  assessment_indices <- purrr::map(resamples$splits, rsample::complement)

  # pass full data to h2o
  full_data_h2o <- h2o::as.h2o(full_data, destination_frame = "grid_data")

  # translate arguments
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
    dplyr::select(parsnip, h2o) %>%
    tidyr::drop_na()

  nm <- original_names$h2o %>%
    rlang::set_names(original_names$parsnip)

  model_args <- rename_list(model_args, nm)
  tuning_args <- nm[tune::tune_args(object)$name]
  tuning_args <- as.character(tuning_args)

  # convert grid to list
  params <- as.list(grid)
  params <- map(params, ~.x[!duplicated(.x)])
  params <- rename_list(params, nm)

  # get model args
  null_args <- sapply(model_args, function(x) is.null(rlang::eval_tidy(x)))
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
    map2(assessment_indices, seq_along(assessment_indices), function(ids, n) {

    if (verbose)
      message(glue::glue("Fitting resample {n}"))

    grid_args <- list(
      grid_id = paste(algorithm, "grid", as.integer(runif(1, 0, 1e9)), sep = "_"),
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
      sort_by = metric,
      decreasing = FALSE
    )
    scores <- as.data.frame(scores@summary_table)
    scores[, ncol(scores)] <- as.numeric(scores[, ncol(scores)])

    for (x in names(params)) {
      scores[[x]] <- gsub("\\[", "", scores[[x]])
      scores[[x]] <- gsub("\\]", "", scores[[x]])
    }

    scores <- scores %>%
      as_tibble() %>%
      dplyr::select(-model_ids) %>%
      dplyr::mutate_at(tuning_args, as.numeric) %>%
      dplyr::mutate(
        .metric = yardstick_name,
        .estimator = if_else(mode == "classification", "multiclass", "standard")
        ) %>%
      dplyr::rename(.estimate = !!metric)

    scores
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

  if (mode == "classification") {
    yardstick_metric <- yardstick::metric_set(mn_log_loss)
  } else {
    yardstick_metric <- yardstick::metric_set(rsq)
  }
  attr(resamples, "metrics") <- yardstick_metric

  resamples
}
