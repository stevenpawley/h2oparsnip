#' library(tidymodels)
#' library(h2oparsnip)
#' library(h2o)
#' h2o.init()
#'
#' object <-
#'   boost_tree(mode = "classification", trees = tune(), learn_rate = tune(), min_n = 3) %>%
#'   set_engine("h2o")
#' rec_obj <- iris %>% recipe(Species ~.)
#' object <- workflow() %>%
#'   add_recipe(rec_obj) %>%
#'   add_model(object)
#' grid <- expand.grid(trees = c(25, 50, 100), learn_rate = c(0.1, 0.3))
#'
#' resamples <- vfold_cv(iris, v = 5)
#'
#' res_tune <- tune_grid(
#'   object,
#'   resamples = resamples,
#'   grid = grid,
#'   metrics = metric_set(accuracy)
#' )
#' attributes(res_tune)
#' collect_metrics(res_tune)
#'
#' res <- tune_grid_h2o(object, resamples = resamples, grid = grid, metrics = "accuracy")
#' attributes(res)
#' collect_metrics(res)
#' select_best(res, metric = "accuracy")
#'
#'
#' #' Tune h2o models
#' #'
#' #' @param object A parsnip model spec object.
#' #' @param preprocessor A recipe object.
#' #' @param resamples A rset object.
#' #' @param grid A tibble of hyperparameter values.
#' #' @param metrics A character vector of metrics to evaluate the tuning results.
#' #'   Available metrics are r2, mean_per_class_accuracy, max_per_class_error,
#' #'   err, total_rows, rmse, accuracy, err_count, logloss, mse,
#' #'   mean_per_class_error
#' #'
#' #' @return
#' #' @export
#' tune_grid_h2o <- function(object, preprocessor = NULL, resamples, grid, metrics) {
#'
#'   # tuning model params only ----
#'   if (inherits(object, "workflow")) {
#'     preprocessor <- workflows::pull_workflow_preprocessor(object)
#'     object <- workflows::pull_workflow_spec(object)
#'   }
#'
#'   # get complete dataset from resamples
#'   data_train <- rsample::training(resamples$splits[[1]])
#'   data_test <- rsample::testing(resamples$splits[[1]])
#'   full_data <- dplyr::bind_rows(data_train, data_test)
#'
#'   row_order <- c(as.numeric(rownames(data_train)), as.numeric(rownames(data_test)))
#'   full_data <- as_tibble(full_data[order(row_order), ])
#'
#'   prepped_recipe <- preprocessor %>% recipes::prep()
#'   full_data <- prepped_recipe %>% recipes::bake(full_data)
#'
#'   # get terms
#'   outcome <- prepped_recipe$term_info %>%
#'     dplyr::filter(role == "outcome") %>%
#'     dplyr::pull(variable)
#'
#'   predictors <- prepped_recipe$term_info %>%
#'     dplyr::filter(role == "predictor") %>%
#'     dplyr::pull(variable)
#'
#'   # get row indexes of assessment set data for each rsplit
#'   assessment_indices <- purrr::map(resamples$splits, rsample::complement)
#'
#'   # pass full data to h2o
#'   full_data_h2o <- h2o::as.h2o(full_data, destination_frame = "grid_data")
#'
#'   # translate arguments
#'   if (inherits(object, "boost_tree")) {
#'     model_name <- "boost_tree"
#'     algorithm <- "gbm"
#'   }
#'   if (inherits(object, "rand_forest")) {
#'     model_name <- "rand_forest"
#'     algorithm <- "randomForest"
#'   }
#'   if (inherits(object, "linear_reg")) {
#'     model_name <- "linear_reg"
#'     algorithm <- "glm"
#'   }
#'   if (inherits(object, "logistic_reg")) {
#'     model_name <- "logistic_reg"
#'     algorithm <- "glm"
#'   }
#'   if (inherits(object, "multinomial_reg")) {
#'     model_name <- "multinomial_reg"
#'     algorithm <- "glm"
#'   }
#'   if (inherits(object, "mlp")) {
#'     model_name <- "mlp"
#'     algorithm <- "deeplearning"
#'   }
#'   if (inherits(object, "naive_Bayes")) {
#'     model_name <- "naive_Bayes"
#'     algorithm <- "naiveBayes"
#'   }
#'
#'   original_names <- translate_args(model_name) %>%
#'     dplyr::select(parsnip, h2o) %>%
#'     tidyr::drop_na()
#'   nm <- original_names$h2o %>%
#'     rlang::set_names(original_names$parsnip)
#'
#'   model_args <- object$args
#'   model_args <- rename_list(model_args, nm)
#'   tuning_args <- nm[tune::tune_args(object)$name]
#'   tuning_args <- as.character(tuning_args)
#'
#'   # convert grid to list
#'   params <- as.list(grid)
#'   params <- map(params, ~.x[!duplicated(.x)])
#'   params <- rename_list(params, nm)
#'
#'   # get model args
#'   null_args <- sapply(model_args, function(x) is.null(rlang::eval_tidy(x)))
#'   model_args <- model_args[!null_args]
#'
#'   # check tunable
#'   if (!any(tuning_args %in% names(params))) {
#'     missing_tune <- tuning_args[!tuning_args %in% names(params)]
#'     rlang::abort(paste("missing arguments", missing_tune))
#'   }
#'
#'   model_args <- model_args[!names(model_args) %in% tuning_args]
#'
#'   if (length(model_args) == 0) {
#'     model_args <- NULL
#'   }
#'
#'   metric_types <- tribble(
#'     ~.h2o_metric, ~.metric, ~.estimator,
#'     "r2", "rsq", "standard",
#'     "mean_per_class_accuracy", "accuracy", "macro",
#'     "rmse", "rmse", "standard",
#'     "accuracy", "accuracy", "multiclass",
#'     "logloss", "mn_log_loss", "multiclass"
#'   )
#'
#'   # fit h2o.grid on each resample
#'   resamples$.metrics <- map(assessment_indices, function(ids) {
#'     grid_args <- list(
#'       grid_id = paste(algorithm, "grid", as.integer(runif(1, 0, 1e9)), sep = "_"),
#'       algorithm = algorithm,
#'       x = predictors,
#'       y = outcome,
#'       training_frame = full_data_h2o[-ids, ],
#'       validation_frame = full_data_h2o[ids, ],
#'       hyper_params = params
#'     )
#'
#'     res <- make_h2o_call("h2o.grid", grid_args, model_args)
#'
#'     scores <- h2o::h2o.getGrid(
#'       grid_id = grid_args$grid_id,
#'       sort_by = metrics
#'     )
#'     scores <- as.data.frame(scores@summary_table)
#'     scores[, ncol(scores)] <- as.numeric(scores[, ncol(scores)])
#'
#'     scores <- scores %>%
#'       as_tibble() %>%
#'       dplyr::select(-model_ids) %>%
#'       dplyr::mutate_at(tuning_args, as.numeric) %>%
#'       dplyr::mutate(.h2o_metric = metrics) %>%
#'       dplyr::left_join(metric_types, by = ".h2o_metric")
#'
#'     names(scores)[length(names(scores))-3] <- ".estimate"
#'
#'     scores
#'   })
#'
#'   class(resamples) <- c("tune_results", class(resamples))
#'   resamples
#' }
