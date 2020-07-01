add_logistic_reg_h2o <- function() {

  parsnip::set_model_engine("logistic_reg", "classification", "h2o")
  parsnip::set_dependency("logistic_reg", "h2o", "h2o")

  parsnip::set_model_arg(
    model = "logistic_reg",
    eng = "h2o",
    parsnip = "mixture",
    original = "alpha",
    func = list(pkg = "dials", fun = "mixture"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "logistic_reg",
    eng = "h2o",
    parsnip = "penalty",
    original = "lambda",
    func = list(pkg = "dials", fun = "penalty"),
    has_submodel = FALSE
  )
  parsnip::set_fit(
    model = "logistic_reg",
    eng = "h2o",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "x", "y", "training_frame", "family"),
      func = c(fun = "h2o_glm_train"),
      defaults = list(
        family = "binomial"
      )
    )
  )
  parsnip::set_encoding(
    model = "logistic_reg",
    eng = "h2o",
    mode = "classification",
    options = list(predictor_indicators = FALSE)
  )

  # classification predict
  parsnip::set_pred(
    model = "logistic_reg",
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
    model = "logistic_reg",
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
    model = "logistic_reg",
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
