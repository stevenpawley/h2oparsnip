# library(tidymodels)
# library(h2oparsnip)
# library(h2o)
# h2o.init()
#
# mod <-
#   boost_tree(mode = "classification", trees = tune(), learn_rate = tune()) %>%
#   set_engine("h2o")
# grid <- expand.grid(trees = c(25, 50, 100), learn_rate = c(0.1, 0.3))
#
# preprocessor <- iris %>%
#   recipe(Species ~.)
#
# resamples <- vfold_cv(iris, v = 5)
#
# res <-
#   tune_grid(
#     object = mod,
#     preprocessor = preprocessor,
#     resamples = resamples,
#     grid = grid,
#     metrics = metric_set(accuracy)
#   )
# res$.metrics
#
# res <- tune_grid_h2o(mod, preprocessor, resamples, grid)
#
# tune_grid_h2o <- function(object, preprocessor, resamples, grid) {
#
#   # tuning model params only ----
#   # get complete dataset from resamples
#   full_data <-
#     bind_rows(analysis(resamples$splits[[1]]), assessment(resamples$splits[[1]]))
#   row_order <- c(as.numeric(rownames(data_train)), as.numeric(rownames(data_test)))
#   full_data <- as_tibble(full_data[order(row_order), ])
#   prepped_recipe <- preprocessor %>% prep()
#   full_data <- prepped_recipe %>% bake(full_data)
#
#   # get terms
#   outcome <- prepped_recipe$term_info %>%
#     filter(role == "outcome") %>%
#     pull(variable)
#   predictors <- prepped_recipe$term_info %>%
#     filter(role == "predictor") %>%
#     pull(variable)
#
#   # get row indexes of assessment set data for each rsplit
#   assessment_indices <- map(resamples$splits, complement)
#
#   # pass full data to h2o
#   full_data_h2o <- as.h2o(full_data, destination_frame = "grid_data")
#
#   # get model arguments
#   model_args <- object$args
#
#   if (inherits(object, "boost_tree")) {
#     algorithm <- "gbm"
#
#     translate_names <- c(
#       trees = "ntrees",
#       max_depth = "max_depth",
#       min_n = "min_rows",
#       learn_rate = "learn_rate",
#       subsample = "sample_rate",
#       mtry = "col_sample_rate",
#       loss_reduction = "min_split_improvement"
#     )
#
#     model_args <- rename_list(model_args, translate_names)
#
#   }
#   if (inherits(object, "rand_forest")) {
#     algorithm <- "randomForest"
#   }
#   if (inherits(object, "linear_reg")) {
#     algorithm <- "glm"
#   }
#   if (inherits(object, "logistic_reg")) {
#     algorithm <- "glm"
#   }
#   if (inherits(object, "multinomial_reg")) {
#     algorithm <- "glm"
#   }
#   if (inherits(object, "mlp")) {
#     algorithm <- "deeplearning"
#   }
#   if (inherits(object, "naive_Bayes")) {
#     algorithm <- "naiveBayes"
#   }
#
#   # convert grid to list
#   params <- as.list(grid)
#   params <- map(params, ~.x[!duplicated(.x)])
#   params <- rename_list(params, translate_names)
#
#   # get model args
#   null_args <- sapply(model_args, function(x) is.null(rlang::eval_tidy(x)))
#   model_args <- model_args[!null_args]
#
#   # check tunable
#   # if (!any(names(model_args) %in% names(params))) {
#   #   missing_tune <- names(model_args)[!names(model_args) %in% names(params)]
#   #   rlang::abort("")
#   # }
#
#   if (length(model_args) == 0)
#     model_args <- NULL
#
#   # fit h2o.grid on each resample
#   resamples$.metrics <- map(assessment_indices, function(ids) {
#     grid_args <- list(
#       grid_id = paste(algorithm, "grid", as.integer(runif(1, 0, 1e9))),
#       algorithm = algorithm,
#       x = predictors,
#       y = outcome,
#       training_frame = full_data_h2o[-ids, ],
#       validation_frame = full_data_h2o[ids, ],
#       hyper_params = params
#     )
#
#     res <- make_h2o_call("h2o.grid", grid_args, model_args)
#
#     scores <- h2o::h2o.getGrid(grid_id = grid_args$grid_id, decreasing = FALSE)
#     scores <- as.data.frame(scores@summary_table)
#     scores <- scores %>%
#       as_tibble() %>%
#       select(-model_ids)
#     scores$.metric <- names(scores)[length(names(scores))]
#     names(scores)[length(names(scores))-1] <- ".estimate"
#
#     scores
#   })
#
#   resamples
# }
