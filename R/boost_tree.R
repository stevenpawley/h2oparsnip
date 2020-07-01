add_boost_tree_h2o <- function() {

  parsnip::set_model_engine("boost_tree", "classification", "h2o")
  parsnip::set_model_engine("boost_tree", "regression", "h2o")
  parsnip::set_dependency("boost_tree", "h2o", "h2o")

  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "h2o",
    parsnip = "trees",
    original = "ntrees",
    func = list(pkg = "dials", fun = "trees"),
    has_submodel = TRUE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "h2o",
    parsnip = "tree_depth",
    original = "max_depth",
    func = list(pkg = "dials", fun = "tree_depth"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "h2o",
    parsnip = "min_n",
    original = "min_rows",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "h2o",
    parsnip = "learn_rate",
    original = "learn_rate",
    func = list(pkg = "dials", fun = "learn_rate"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "h2o",
    parsnip = "sample_size",
    original = "sample_rate",
    func = list(pkg = "dials", fun = "sample_size"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "h2o",
    parsnip = "mtry",
    original = "col_sample_rate",
    func = list(pkg = "dials", fun = "mtry"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "h2o",
    parsnip = "loss_reduction",
    original = "min_split_improvement",
    func = list(pkg = "dials", fun = "loss_reduction"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "h2o",
    parsnip = "stop_iter",
    original = "stopping_rounds",
    func = list(pkg = "dials", fun = "stop_iter"),
    has_submodel = FALSE
  )
  parsnip::set_fit(
    model = "boost_tree",
    eng = "h2o",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "x", "y", "training_frame"),
      func = c(fun = "h2o_gbm_train"),
      defaults = list()
    )
  )
  parsnip::set_fit(
    model = "boost_tree",
    eng = "h2o",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "x", "y", "training_frame"),
      func = c(fun = "h2o_gbm_train"),
      defaults = list()
    )
  )
  parsnip::set_encoding(
    model = "boost_tree",
    eng = "h2o",
    mode = "classification",
    options = list(predictor_indicators = FALSE)
  )
  parsnip::set_encoding(
    model = "boost_tree",
    eng = "h2o",
    mode = "regression",
    options = list(predictor_indicators = FALSE)
  )

  # regression predict
  parsnip::set_pred(
    model = "boost_tree",
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
    model = "boost_tree",
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
    model = "boost_tree",
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
    model = "boost_tree",
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
    model = "boost_tree",
    eng = "h2o",
    mode = "classification",
    type = "raw",
    value = list(
      pre = function(x, object) h2o::as.h2o(x),
      post = function(x, object) as.data.frame(x),
      func = c(pkg = "h2o", fun = "h2o.predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data))
    )
  )

}

#' Wrapper for training a h2o.gbm model as part of a parsnip `boost_tree`
#' h2o engine
#'
#' @param formula formula
#' @param data data.frame of training data
#' @param ntrees integer, the number of trees to build (default = 50).
#' @param max_depth integer, the maximum tree depth (default = 10).
#' @param min_rows integer, the minimum number of observations for a leaf
#'   (default = 10).
#' @param learn_rate numeric, the learning rate (default = 0.1, range is from
#'   0.0 to 1.0).
#' @param sample_rate numeric, the proportion of samples to use to build each
#'   tree (default = 1.0).
#' @param col_sample_rate numeric, the proportion of features available during
#'   each node split (default = 1.0).
#' @param min_split_improvement numeric,  minimum relative improvement in
#'   squared error reduction in order for a split to happen (default = 1e-05)
#' @param stopping_rounds An integer specifying the number of training
#'   iterations without improvement before stopping. If `stopping_rounds = 0`
#'   (the default) then early stopping is disabled.  If `validation` is used,
#'   performance is base on the validation set; otherwise the training set is
#'   used.
#' @param validation A positive number. If on `[0, 1)` the value, `validation`
#' is a random proportion of data in `x` and `y` that are used for performance
#' assessment and potential early stopping. If 1 or greater, it is the _number_
#' of training set samples use for these purposes.
#' @param ... other arguments not currently used.
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

    # convert to h2oframe
    if (!inherits(data, "H2OFrame"))
      data <- h2o::as.h2o(data)

    if (!is.null(valid)) {
      valid <- h2o::as.h2o(valid)
    }

    # convert mtry (number of features) to proportions
    if (col_sample_rate > 1)
      col_sample_rate <- col_sample_rate / length(X)

    # define arguments
    args <- list(
      x = X,
      y = y,
      training_frame = data,
      validation_frame = valid,
      ntrees = ntrees,
      max_depth = max_depth,
      min_rows = min_rows,
      learn_rate = learn_rate,
      sample_rate = sample_rate,
      col_sample_rate = col_sample_rate,
      min_split_improvement = min_split_improvement,
      stopping_rounds = stopping_rounds
    )

    res <- make_h2o_call("h2o.gbm", args, others)

    res
  }
