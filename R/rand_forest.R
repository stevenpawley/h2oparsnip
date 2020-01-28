#' Wrapper for training a h2o.randomForest model as part of a parsnip
#' `rand_forest` h2o engine
#'
#' @param formula formula
#' @param data data.frame of training data
#' @param ntrees integer, the number of trees to build (default = 50)
#' @param min_rows integer, the minimum number of observations for a leaf
#'   (default = 10)
#' @param mtries integer, the number of columns to randomly select at each
#' level. Default of -1 is sqrt(p) for classification and (p/3) for regression.
#' @param ... other arguments not currently used
#'
#' @return evaluated h2o model call
#' @export
h2o_rf_train <-
  function(formula,
           data,
           ntrees = 50,
           min_rows = 10,
           mtries = -1,
           ...) {

    # extract terms
    X <- extract_terms(formula, data)$X
    y <- extract_terms(formula, data)$y

    # define H2OFrame
    if (!inherits(data, "H2OFrame"))
      data <- h2o::as.h2o(data)

    # define arguments
    args <- list(
      x = X,
      y = y,
      training_frame = data,
      ntrees = ntrees,
      min_rows = min_rows,
      mtries = mtries
    )

    others <- list(...)

    # create unevaluated model call
    args <- args[lapply(args, length) > 0]

    model_call <- rlang::call2(.fn = "h2o.randomForest", !!!args, .ns = "h2o")

    if (length(others) > 0)
      model_call <- rlang::call_modify(model_call, !!!others)

    rlang::eval_tidy(model_call)
  }
