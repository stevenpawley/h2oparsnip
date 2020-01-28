#' Wrapper for training a h2o.gbm model as part of a parsnip `boost_tree`
#' h2o engine
#'
#' @param formula formula
#' @param data data.frame of training data
#' @param ntrees integer, the number of trees to build (default = 50)
#' @param max_depth integer, the maximum tree depth (default = 10)
#' @param min_rows integer, the minimum number of observations for a leaf
#'   (default = 10)
#' @param learn_rate numeric, the learning rate (default = 0.1, range is from
#'   0.0 to 1.0)
#' @param sample_rate numeric, the proportion of samples to use to build each
#'   tree (default = 1.0)
#' @param col_sample_rate numeric, the proportion of features available during
#'   each node split (default = 1.0)
#' @param min_split_improvement numeric,  minimum relative improvement in
#'   squared error reduction in order for a split to happen (default = 1e-05)
#' @param ... other arguments not currently used
#'
#' @return evaluated h2o model call
#' @export
h2o_gbm_train <-
  function(formula,
           data,
           ntrees = 50,
           max_depth = 5,
           min_rows = 10,
           learn_rate = 0.1,
           sample_rate = 1.0,
           col_sample_rate = 1.0,
           min_split_improvement = 1e-05,
           ...) {

    # extract terms
    X <- extract_terms(formula, data)$X
    y <- extract_terms(formula, data)$y

    # define H2OFrame
    if (!inherits(data, "H2OFrame"))
      data <- h2o::as.h2o(data)

    # convert mtry (number of features) and min_rows to proportions
    if (col_sample_rate > 1) {
      col_sample_rate <- col_sample_rate / length(X)
    }

    # define arguments
    args <- list(
      x = X,
      y = y,
      training_frame = data,
      ntrees = ntrees,
      max_depth = max_depth,
      min_rows = min_rows,
      learn_rate = learn_rate,
      sample_rate = sample_rate,
      col_sample_rate = col_sample_rate,
      min_split_improvement = min_split_improvement
    )

    others <- list(...)

    # create unevaluated model call
    args <- args[lapply(args, length) > 0]

    model_call <- rlang::call2(.fn = "h2o.gbm", !!!args, .ns = "h2o")

    if (length(others) > 0)
      model_call <- rlang::call_modify(model_call, !!!others)

    rlang::eval_tidy(model_call)
  }
