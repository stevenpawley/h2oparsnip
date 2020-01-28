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
      defaults = list()
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
      defaults = list()
    )
  )

  # regression predict
  parsnip::set_pred(
    model = "rand_forest",
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
    model = "rand_forest",
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
    model = "rand_forest",
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
    model = "rand_forest",
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
    model = "rand_forest",
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
