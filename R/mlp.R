#' Parsnip model specification
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
h2o_train <-
  function(formula,
           data,
           l2 = 0,
           hidden_dropout_ratios = 0,
           hidden = 200,
           epochs = 10,
           activation = "Rectifier",
           ...) {

    # check that formula is two-sided
    if (!attr(stats::terms(formula, data = data), "response"))
      stop("Two-sided formula is required")

    # extract terms
    X <- attr(stats::terms(formula, data = data), "term.labels")
    y <- all.vars(formula)[1]

    # define H2OFrame
    if (!inherits(data, "H2OFrame"))
      data <- h2o::as.h2o(data)

    # remap dials::values_activation to permissible h2o activation values
    if (activation %in% c("linear", "elu", "softmax"))
      stop(paste(activation, "activation function is not available when using the h2o engine"))

    if (activation == "relu")
      activation <- "Rectifier"

    if (activation == "Rectifier" & hidden_dropout_ratios > 0) {
      activation <- "RectifierWithDropout"

    } else if (activation == "Tanh" & hidden_dropout_ratios > 0) {
      activation <- "TanhWithDropout"

    } else if (activation == "Maxout" & hidden_dropout_ratios > 0) {
      activation <- "MaxoutWithDropout"
    }

    if (hidden_dropout_ratios == 0)
      hidden_dropout_ratios <- NULL

    # define arguments
    args <- list(
      x = X,
      y = y,
      training_frame = data,
      l2 = l2,
      hidden_dropout_ratios = hidden_dropout_ratios,
      hidden = hidden,
      epochs = epochs,
      activation = activation
    )

    others <- list(...)

    # create unevaluated model call
    args <- args[lapply(args, length) > 0]

    model_call <- rlang::call2(.fn = "h2o.deeplearning", !!!args, .ns = "h2o")

    if (length(others) > 0)
      model_call <- rlang::call_modify(model_call, !!!others)

    rlang::eval_tidy(model_call)
}


#' Wrapper for prediction for h2o mlp model
#'
#' @param object H2O model
#' @param newdata data to predict
#' @param ... currently unused
#'
#' @return tibble
#' @export
h2o_pred <- function(object, newdata, ...) {

  # convert to H2OFrame
  if (!inherits(newdata, "H2OFrame"))
    newdata <- h2o::as.h2o(newdata)

  res <- stats::predict(object, newdata, ...)
  tibble::as_tibble(res)
}

