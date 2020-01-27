#' Wrapper for training a h2o.gbm model as part of a parsnip `boost_tree`
#' h2o engine
#'
#' @param formula formula
#' @param data data.frame of training data
#' @param l2 numeric, l2 regulation parameter, default = 0
#' @param hidden_dropout_ratios dropout ratio for a single hidden layer (default
#'   = 0)
#' @param hidden integer, number of neurons in the hidden layer (default = 200)
#' @param epochs integer, number of epochs (default = 10)
#' @param activation character, activation function. Must be one of: "Tanh",
#'   "TanhWithDropout", "Rectifier", "RectifierWithDropout", "Maxout",
#'   "MaxoutWithDropout". Defaults to "Rectifier. If `hidden_dropout_ratios` > 0
#'   then the equivalent activation function with dropout is used.
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

    if (min_rows > 1) {
      min_rows <- min_rows / nrow(data)
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
      col_sample_rate = col_sample_rate
    )

    others <- list(...)

    # create unevaluated model call
    args <- args[lapply(args, length) > 0]

    model_call <- rlang::call2(.fn = "h2o.gbm", !!!args, .ns = "h2o")

    if (length(others) > 0)
      model_call <- rlang::call_modify(model_call, !!!others)

    rlang::eval_tidy(model_call)
  }
