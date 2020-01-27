#' Wrapper to add the `h2o` engine to the parsnip `mlp` model specification
#'
#' @return NULL
#' @export
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
      protect = c("data"),
      func = c(fun = "h2o_train"),
      defaults = list()
    )
  )

  parsnip::set_fit(
    model = "mlp",
    eng = "h2o",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("data"),
      func = c(fun = "h2o_train"),
      defaults = list()
    )
  )

  parsnip::set_pred(
    model = "mlp",
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
    model = "mlp",
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
    model = "mlp",
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
    model = "mlp",
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
    model = "mlp",
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
