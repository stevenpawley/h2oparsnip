#' Wrapper to add the `h2o` engine to the parsnip `multinom_reg` model
#' specification
#'
#' @return NULL
#' @export
add_multinom_reg_h2o <- function() {

  parsnip::set_model_engine("multinom_reg", "classification", "h2o")
  parsnip::set_dependency("multinom_reg", "h2o", "h2o")

  parsnip::set_model_arg(
    model = "multinom_reg",
    eng = "h2o",
    parsnip = "mixture",
    original = "alpha",
    func = list(pkg = "dials", fun = "mixture"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "multinom_reg",
    eng = "h2o",
    parsnip = "penalty",
    original = "lambda",
    func = list(pkg = "dials", fun = "penalty"),
    has_submodel = FALSE
  )
  parsnip::set_fit(
    model = "multinom_reg",
    eng = "h2o",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "x", "y", "training_frame", "family"),
      func = c(fun = "h2o_glm_train"),
      defaults = list(family = "multinomial")
    )
  )

  # classification predict
  parsnip::set_pred(
    model = "multinom_reg",
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
    model = "multinom_reg",
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
    model = "multinom_reg",
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
