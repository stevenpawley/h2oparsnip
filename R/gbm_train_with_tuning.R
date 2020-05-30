#' #' Wrapper for training a h2o.gbm model as part of a parsnip `boost_tree`
#' #' h2o engine
#' #'
#' #' @param formula formula
#' #' @param data data.frame of training data
#' #' @param model_id A randomly assigned identifier for the model. Used to refer
#' #'   to the model within the h2o cluster.
#' #' @param ntrees integer, the number of trees to build (default = 50).
#' #' @param max_depth integer, the maximum tree depth (default = 10).
#' #' @param min_rows integer, the minimum number of observations for a leaf
#' #'   (default = 10).
#' #' @param learn_rate numeric, the learning rate (default = 0.1, range is from
#' #'   0.0 to 1.0).
#' #' @param sample_rate numeric, the proportion of samples to use to build each
#' #'   tree (default = 1.0).
#' #' @param col_sample_rate numeric, the proportion of features available during
#' #'   each node split (default = 1.0).
#' #' @param min_split_improvement numeric,  minimum relative improvement in
#' #'   squared error reduction in order for a split to happen (default = 1e-05)
#' #' @param ... other arguments not currently used.
#' #'
#' #' @return evaluated h2o model call
#' #' @export
#' h2o_gbm_train <-
#'   function(formula,
#'            data,
#'            model_id,
#'            ntrees = 50,
#'            max_depth = 5,
#'            min_rows = 10,
#'            learn_rate = 0.1,
#'            sample_rate = 1.0,
#'            col_sample_rate = 1.0,
#'            min_split_improvement = 1e-05,
#'            ...) {
#'
#'     others <- list(...)
#'
#'     # convert to H2OFrame and get response/predictor names
#'     dest_frame <- paste("training_data", model_id, sep = "_")
#'     pre <- preprocess_training(formula, data, dest_frame)
#'
#'     # convert mtry (number of features) to proportions
#'     if (col_sample_rate > 1) {
#'       col_sample_rate <- col_sample_rate / length(pre$X)
#'     }
#'
#'     model_args <- list(
#'       ntrees = ntrees,
#'       max_depth = max_depth,
#'       min_rows = min_rows,
#'       learn_rate = learn_rate,
#'       sample_rate = sample_rate,
#'       col_sample_rate = col_sample_rate,
#'       min_split_improvement = min_split_improvement
#'     )
#'
#'     # hyperparameter tuning
#'     best_params <- NULL
#'
#'     if ("hyper_params" %in% names(others)) {
#'       translate_names <- c(
#'         trees = "ntrees",
#'         max_depth = "max_depth",
#'         min_n = "min_rows",
#'         learn_rate = "learn_rate",
#'         subsample = "sample_rate",
#'         mtry = "col_sample_rate",
#'         loss_reduction = "min_split_improvement"
#'       )
#'
#'       grid_args <- model_args
#'       grid_args$algorithm <- "gbm"
#'       grid_args$grid_id <- gsub("h2o_gbm_train", "h2o_gbm_grid", model_id)
#'       grid_args$x <- pre$X
#'       grid_args$y <- pre$y
#'       grid_args$training_frame = pre$data
#'       grid_args$hyper_params <-
#'         rlang::eval_tidy(rename_list(others$hyper_params, translate_names))
#'       grid_args$nfolds <- others$nfolds
#'
#'       res <- make_h2o_call("h2o.grid", grid_args, others = NULL)
#'
#'       scores <- h2o::h2o.getGrid(grid_id = grid_args$grid_id, decreasing = FALSE)
#'       scores <- as.data.frame(scores@summary_table)
#'       best_model_id <- scores$model_ids[1]
#'       best_params <- scores[1, !names(scores) %in% c("model_ids", "logloss")]
#'       best_params <- as.list(best_params)
#'       best_params <- map(best_params, as.numeric)
#'     }
#'
#'     # define arguments
#'     args <- list(
#'       model_id = model_id,
#'       x = pre$X,
#'       y = pre$y,
#'       training_frame = pre$data,
#'       ntrees = ntrees,
#'       max_depth = max_depth,
#'       min_rows = min_rows,
#'       learn_rate = learn_rate,
#'       sample_rate = sample_rate,
#'       col_sample_rate = col_sample_rate,
#'       min_split_improvement = min_split_improvement
#'     )
#'
#'     args <- list_modify(args, !!!best_params)
#'     others <- others[!names(others) %in% c("hyper_params", "nfolds")]
#'     if (length(others) == 0) others <- NULL
#'
#'     res <- make_h2o_call("h2o.gbm", args, others)
#'
#'     res
#'   }
