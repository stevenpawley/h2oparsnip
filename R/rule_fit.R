add_rule_fit_h2o <- function() {

  parsnip::set_model_engine("rule_fit", "classification", "h2o")
  parsnip::set_model_engine("rule_fit", "regression", "h2o")
  parsnip::set_dependency("rule_fit", "h2o", "h2o")

  parsnip::set_model_arg(
    model = "rule_fit",
    eng = "h2o",
    parsnip = "trees",
    original = "rule_generation_ntrees",
    func = list(pkg = "dials", fun = "trees"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "rule_fit",
    eng = "h2o",
    parsnip = "tree_depth",
    original = "max_rule_length",
    func = list(pkg = "dials", fun = "tree_depth"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "rule_fit",
    eng = "h2o",
    parsnip = "penalty",
    original = "lambda",
    func = list(pkg = "dials", fun = "penalty"),
    has_submodel = FALSE
  )
  parsnip::set_fit(
    model = "rule_fit",
    eng = "h2o",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "x", "y", "training_frame"),
      func = c(fun = "h2o_rulefit_train"),
      defaults = list()
    )
  )
  parsnip::set_fit(
    model = "rule_fit",
    eng = "h2o",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "x", "y", "training_frame"),
      func = c(fun = "h2o_rulefit_train"),
      defaults = list()
    )
  )
  parsnip::set_encoding(
    model = "rule_fit",
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
    model = "rule_fit",
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
    model = "rule_fit",
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
    model = "rule_fit",
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
    model = "rule_fit",
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
    model = "rule_fit",
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
    model = "rule_fit",
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

#' Wrapper for training a h2o.rulefit model as part of a parsnip
#' `rule_fit` h2o engine
#'
#' @param formula formula
#' @param data data.frame of training data
#' @param rule_generation_ntrees integer, the number of trees to build (default
#'   = 50)
#' @param max_rule_length integer, the maximum tree depth (default = 3).
#' @param lambda Specify the regularization strength for LASSO regressor.
#' @param ... other arguments that are passed to the h2o model
#'
#' @return evaluated h2o model call
#' @export
h2o_rulefit_train <-
  function(formula,
           data,
           rule_generation_ntrees = 50,
           max_rule_length = 3,
           lambda = 0,
           ...) {

    others <- list(...)

    # get term names and convert to h2o
    X <- attr(stats::terms(formula, data = data), "term.labels")
    y <- all.vars(formula)[1]

    # convert to H2OFrame (although parsnip doesn't support H2OFrames right now)
    if (!inherits(data, "H2OFrame"))
      data <- h2o::as.h2o(data)

    # define arguments
    args <- list(
      x = X,
      y = y,
      training_frame = data,
      rule_generation_ntrees = rule_generation_ntrees,
      max_rule_length = max_rule_length,
      lambda = lambda
    )

    res <- make_h2o_call("h2o.rulefit", args, others)

    res
  }
