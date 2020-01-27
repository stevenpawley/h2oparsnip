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
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "h2o",
    parsnip = "tree_depth",
    original = "max_depth",
    func = list(pkg = "dials", fun = "max_depth"),
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

  # regression predict
  parsnip::set_pred(
    model = "boost_tree",
    eng = "h2o",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = function(x, object) {
        x$predict
      },
      func = c(fun = "h2o_pred"),
      args =
        list(
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
      pre = NULL,
      post = function(x, object) {
        x$predict
      },
      func = c(fun = "h2o_pred"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data))
    )
  )

  parsnip::set_pred(
    model = "boost_tree",
    eng = "h2o",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = function(x, object) {
        object$lvl[apply(x[, 2:ncol(x)], 1, which.max)]
      },
      func = c(fun = "h2o_pred"),
      args =
        list(
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
      pre = NULL,
      post = function(x, object) {
        x[, 2:ncol(x)]
      },
      func = c(fun = "h2o_pred"),
      args =
        list(
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
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data))
    )
  )
}
