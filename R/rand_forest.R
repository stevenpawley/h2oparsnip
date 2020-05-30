#' Wrapper to add the `h2o` engine to the parsnip `rand_forest` model
#' specification
#'
#' @return NULL
#' @export
add_rand_forest_h2o <- function() {

  parsnip::set_model_engine("rand_forest", "classification", "h2o")
  parsnip::set_model_engine("rand_forest", "regression", "h2o")
  parsnip::set_dependency("rand_forest", "h2o", "h2o")

  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "h2o",
    parsnip = "trees",
    original = "ntrees",
    func = list(pkg = "dials", fun = "trees"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "h2o",
    parsnip = "min_n",
    original = "min_rows",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "h2o",
    parsnip = "mtry",
    original = "mtries",
    func = list(pkg = "dials", fun = "mtry"),
    has_submodel = FALSE
  )
  parsnip::set_fit(
    model = "rand_forest",
    eng = "h2o",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "x", "y", "training_frame"),
      func = c(fun = "h2o_rf_train"),
      defaults = list(
        model_id = paste("rand_forest", as.integer(runif(1, 0, 1e9)), sep = "_")
      )
    )
  )
  parsnip::set_fit(
    model = "rand_forest",
    eng = "h2o",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "x", "y", "training_frame"),
      func = c(fun = "h2o_rf_train"),
      defaults = list(
        model_id = paste("rand_forest", as.integer(runif(1, 0, 1e9)), sep = "_")
      )
    )
  )

  # regression predict
  parsnip::set_pred(
    model = "rand_forest",
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
    model = "rand_forest",
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
    model = "rand_forest",
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
    model = "rand_forest",
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
    model = "rand_forest",
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

#' Wrapper for training a h2o.randomForest model as part of a parsnip
#' `rand_forest` h2o engine
#'
#' @param formula formula
#' @param data data.frame of training data
#' @param model_id A randomly assigned identifier for the model. Used to refer
#'   to the model within the h2o cluster.
#' @param ntrees integer, the number of trees to build (default = 50)
#' @param min_rows integer, the minimum number of observations for a leaf
#'   (default = 10)
#' @param mtries integer, the number of columns to randomly select at each
#' level. Default of -1 is sqrt(p) for classification and (p/3) for regression.
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
h2o_rf_train <-
  function(formula,
           data,
           model_id,
           ntrees = 50,
           min_rows = 10,
           mtries = -1,
           stopping_rounds = 0,
           validation = 0,
           ...) {

    others <- list(...)

    # early stopping
    if (stopping_rounds > 0 & validation > 0) {
      n <- nrow(data)
      trn_index <- sample(1:n, size = floor(n * validation) + 1)
      valid <- h2o::as.h2o(data[-trn_index, ])
      data <- data[trn_index, ]
    } else {
      valid <- NULL
    }

    # convert to H2OFrame, get response and predictor names
    dest_frame <- paste("training_data", model_id, sep = "_")
    pre <- preprocess_training(formula, data, dest_frame)

    # check for valid mtries
    if (mtries > length(pre$X)) {
      mtries <- length(pre$X)
    }

    # define arguments
    args <- list(
      model_id = model_id,
      x = pre$X,
      y = pre$y,
      training_frame = pre$data,
      validation_frame = valid,
      ntrees = ntrees,
      min_rows = min_rows,
      mtries = mtries,
      stopping_rounds = stopping_rounds
    )

    res <- make_h2o_call("h2o.randomForest", args, others)

    res
  }
