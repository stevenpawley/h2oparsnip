add_automl <- function() {

  # define model
  parsnip::set_new_model("automl")

  # define model modes
  parsnip::set_model_mode(model = "automl", mode = "classification")
  parsnip::set_model_engine("automl", mode = "classification", eng = "h2o")

  parsnip::set_model_mode(model = "automl", mode = "regression")
  parsnip::set_model_engine("automl", mode = "regression", eng = "h2o")

  # define dependencies for each mode
  parsnip::set_dependency("automl", "h2o", "h2o")

  # define fit methods
  parsnip::set_fit(
    model = "automl",
    eng = "h2o",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "x", "y", "training_frame"),
      func = c(fun = "h2o_automl_train"),
      defaults = list()
    )
  )
  parsnip::set_fit(
    model = "automl",
    eng = "h2o",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "x", "y", "training_frame"),
      func = c(fun = "h2o_automl_train"),
      defaults = list()
    )
  )

  # regression predict
  parsnip::set_pred(
    model = "automl",
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
    model = "automl",
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
    model = "automl",
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
    model = "automl",
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
    model = "automl",
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


#' Wrapper for training a h2o.automl model
#'
#' @param formula formula
#' @param data data.frame of training data
#' @param ... Other arguments to pass the h2o.automl
#'
#' @return evaluated h2o model call
#' @export
h2o_automl_train <- function(formula, data, ...) {
  others <- list(...)

  # convert to a H2OFrame and split response and predictor names
  pre <- preprocess_training(formula, data)

  # define arguments
  args <- list(
    x = pre$X,
    y = pre$y,
    training_frame = pre$data
  )

  others <- list(...)
  make_h2o_call("h2o.automl", args, others)
}
