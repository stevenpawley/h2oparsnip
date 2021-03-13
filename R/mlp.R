add_mlp_h2o <- function() {

  parsnip::set_model_engine("mlp", "classification", "h2o")
  parsnip::set_model_engine("mlp", "regression", "h2o")
  parsnip::set_dependency("mlp", "h2o", "h2o")

  parsnip::set_model_arg(
    model = "mlp",
    eng = "h2o",
    parsnip = "cost",
    original = "l2",
    func = list(pkg = "dials", fun = "cost"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "mlp",
    eng = "h2o",
    parsnip = "dropout",
    original = "hidden_dropout_ratios",
    func = list(pkg = "dials", fun = "dropout"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "mlp",
    eng = "h2o",
    parsnip = "hidden_units",
    original = "hidden",
    func = list(pkg = "dials", fun = "hidden_units"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "mlp",
    eng = "h2o",
    parsnip = "epochs",
    original = "epochs",
    func = list(pkg = "dials", fun = "epochs"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "mlp",
    eng = "h2o",
    parsnip = "activation",
    original = "activation",
    func = list(pkg = "dials", fun = "activation"),
    has_submodel = FALSE
  )
  parsnip::set_fit(
    model = "mlp",
    eng = "h2o",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "x", "y", "training_frame"),
      func = c(fun = "h2o_mlp_train"),
      defaults = list()
    )
  )
  parsnip::set_fit(
    model = "mlp",
    eng = "h2o",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "x", "y", "training_frame"),
      func = c(fun = "h2o_mlp_train"),
      defaults = list()
    )
  )
  parsnip::set_encoding(
    model = "mlp",
    eng = "h2o",
    mode = "classification",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )
  parsnip::set_encoding(
    model = "mlp",
    eng = "h2o",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  # regression predict
  parsnip::set_pred(
    model = "mlp",
    eng = "h2o",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = function(x, object) h2o::as.h2o(x),
      post = function(x, object) as.data.frame(x)$predict,
      func = c(pkg = "h2o", fun = "h2o.predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data)
      )
    )
  )
  parsnip::set_pred(
    model = "mlp",
    eng = "h2o",
    mode = "regression",
    type = "raw",
    value = list(
      pre = function(x, object) h2o::as.h2o(x),
      post = function(x, object) as.data.frame(x),
      func = c(pkg = "h2o", fun = "h2o.predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data)
      )
    )
  )

  # classification predict
  parsnip::set_pred(
    model = "mlp",
    eng = "h2o",
    mode = "classification",
    type = "class",
    value = list(
      pre = function(x, object) h2o::as.h2o(x),
      post = function(x, object) as.data.frame(x)$predict,
      func = c(pkg = "h2o", fun = "h2o.predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data)
      )
    )
  )
  parsnip::set_pred(
    model = "mlp",
    eng = "h2o",
    mode = "classification",
    type = "prob",
    value = list(
      pre = function(x, object) h2o::as.h2o(x),
      post = function(x, object) as.data.frame(x[, 2:ncol(x)]),
      func = c(pkg = "h2o", fun = "h2o.predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data)
      )
    )
  )
  parsnip::set_pred(
    model = "mlp",
    eng = "h2o",
    mode = "classification",
    type = "raw",
    value = list(
      pre = function(x, object) h2o::as.h2o(x),
      post = function(x, object) as.data.frame(x),
      func = c(pkg = "h2o", fun = "h2o.predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data)
      )
    )
  )
}

#' Wrapper for training a h2o.deeplearning model as part of a parsnip `mlp`
#' h2o engine
#'
#' @param formula formula
#' @param data data.frame of training data
#' @param l2 numeric, l2 regulation parameter, default = 0
#' @param hidden_dropout_ratios dropout ratio for a single hidden layer (default
#'   = 0)
#' @param hidden integer, number of neurons in the hidden layer (default = c(200, 200))
#' @param epochs integer, number of epochs (default = 10)
#' @param activation character, activation function. Must be one of: "Tanh",
#'   "TanhWithDropout", "Rectifier", "RectifierWithDropout", "Maxout",
#'   "MaxoutWithDropout". Defaults to "Rectifier. If `hidden_dropout_ratios` > 0
#'   then the equivalent activation function with dropout is used.
#' @param stopping_rounds An integer specifying the number of training
#'   iterations without improvement before stopping. If `stopping_rounds = 0`
#'   (the default) then early stopping is disabled.  If `validation` is used,
#'   performance is base on the validation set; otherwise the training set is
#'   used.
#' @param validation A positive number. If on `[0, 1)` the value, `validation`
#' is a random proportion of data in `x` and `y` that are used for performance
#' assessment and potential early stopping. If 1 or greater, it is the _number_
#' of training set samples use for these purposes.
#' @param ... other arguments not currently used
#'
#' @return evaluated h2o model call
#' @export
h2o_mlp_train <-
  function(formula,
           data,
           l2 = 0,
           hidden_dropout_ratios = 0,
           hidden = 100,
           epochs = 10,
           activation = "Rectifier",
           stopping_rounds = 0,
           validation = 0,
           ...) {

    others <- list(...)

    # get term names and convert to h2o
    X <- attr(stats::terms(formula, data = data), "term.labels")
    y <- all.vars(formula)[1]

    # early stopping
    if (validation > 1)
      validation <- validation / nrow(data)

    if (stopping_rounds > 0 & validation > 0) {
      n <- nrow(data)
      trn_index <- sample(1:n, size = floor(n * validation) + 1)
      valid <- data[-trn_index, ]
      data <- data[trn_index, ]
    } else {
      valid <- NULL
    }

    # convert to H2OFrame (although parsnip doesn't support H2OFrames right now)
    if (!inherits(data, "H2OFrame"))
      data <- h2o::as.h2o(data)

    if (!is.null(valid)) {
      valid <- h2o::as.h2o(valid)
    }

    # remap dials::values_activation to permissible h2o activation values
    if (activation %in% c("linear", "elu", "softmax")) {
      stop(
        paste(
          activation,
          "activation function is not available when using the h2o engine.")
      )
    }

    activation <- switch(
      activation,
      relu = "Rectifier",
      tanh = "Tanh",
      maxout = "Maxout",
      activation
    )

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
      validation_frame = valid,
      l2 = l2,
      hidden_dropout_ratios = hidden_dropout_ratios,
      hidden = hidden,
      epochs = epochs,
      activation = activation,
      stopping_rounds = stopping_rounds
    )

    make_h2o_call("h2o.deeplearning", args, others)
  }
