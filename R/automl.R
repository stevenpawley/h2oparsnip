#' General interface for automl models
#'
#' @param mode A single character string for the type of model.
#'
#' @return A model_spec
#' @export
automl <- function(mode = "classification") {
  args <- list()

  new_model_spec(
    "automl",
    args = args,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = "h2o"
  )
}

#' @export
print.automl <- function(x, ...) {
  cat("Automl Model Specification (", x$mode, ")\n\n", sep = "")
  parsnip::model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(parsnip::show_call(x))
  }

  invisible(x)
}


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
      defaults = list(
        model_id = paste("automl", as.integer(runif(1, 0, 1e9)), sep = "_")
      )
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
      defaults = list(
        model_id = paste("automl", as.integer(runif(1, 0, 1e9)), sep = "_")
      )
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
#' @param model_id A randomly assigned identifier for the model. Used to refer
#'   to the model within the h2o cluster.
#' @param ... Other arguments to pass the h2o.automl
#'
#' @return evaluated h2o model call
#' @export
h2o_automl_train <- function(formula, data, model_id, ...) {
  others <- list(...)

  # convert to a H2OFrame and split response and predictor names
  pre <- preprocess_training(formula, data)

  # define arguments
  args <- list(
    model_id = model_id,
    x = pre$X,
    y = pre$y,
    training_frame = pre$data
  )

  others <- list(...)
  make_h2o_call("h2o.automl", args, others)
}

