#' Wrapper to add the `h2o` engine to the parsnip `boost_tree` model
#' specification
#'
#' @return NULL
#' @export
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
  parsnip::set_fit(
    model = "boost_tree",
    eng = "h2o",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "x", "y", "training_frame"),
      func = c(fun = "h2o_gbm_train"),
      defaults = list(
        model_id = paste("gbm", as.integer(runif(1, 0, 1e9)), sep = "_")
      )
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
      defaults = list(
        model_id = paste("gbm", as.integer(runif(1, 0, 1e9)), sep = "_")
      )
    )
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
#' @param model_id A randomly assigned identifier for the model. Used to refer
#'   to the model within the h2o cluster.
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
#' @param ... other arguments not currently used.
#'
#' @return evaluated h2o model call
#' @export
h2o_gbm_train <-
  function(formula,
           data,
           model_id,
           ntrees = 50,
           max_depth = 5,
           min_rows = 10,
           learn_rate = 0.1,
           sample_rate = 1.0,
           col_sample_rate = 1.0,
           min_split_improvement = 1e-05,
           ...) {

    others <- list(...)

    # convert to H2OFrame, get response and predictor names
    dest_frame <- paste("training_data", model_id, sep = "_")
    pre <- preprocess_training(formula, data, dest_frame)

    # convert mtry (number of features) to proportions
    if (col_sample_rate > 1) {
      col_sample_rate <- col_sample_rate / length(pre$X)
    }

    # define arguments
    args <- list(
      model_id = model_id,
      x = pre$X,
      y = pre$y,
      training_frame = pre$data,
      ntrees = ntrees,
      max_depth = max_depth,
      min_rows = min_rows,
      learn_rate = learn_rate,
      sample_rate = sample_rate,
      col_sample_rate = col_sample_rate,
      min_split_improvement = min_split_improvement
    )

    res <- make_h2o_call("h2o.gbm", args, others)

    res
  }
