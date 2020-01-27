#' Wrapper to add the `h2o` engine to the parsnip `mlp` model specification
#'
#' @return NULL
#' @export
#' @importFrom parsnip set_model_engine set_dependency set_model_arg set_fit
#' set_pred
add_h2o_engine <- function() {
  set_model_engine("mlp", "classification", "h2o")
  set_model_engine("mlp", "regression", "h2o")
  set_dependency("mlp", "h2o", "h2o")

  set_model_arg(
    model = "mlp",
    eng = "h2o",
    parsnip = "cost",
    original = "l2",
    func = list(pkg = "dials", fun = "cost"),
    has_submodel = FALSE
  )

  set_model_arg(
    model = "mlp",
    eng = "h2o",
    parsnip = "dropout",
    original = "hidden_dropout_ratios",
    func = list(pkg = "dials", fun = "dropout"),
    has_submodel = FALSE
  )

  set_model_arg(
    model = "mlp",
    eng = "h2o",
    parsnip = "hidden_units",
    original = "hidden",
    func = list(pkg = "dials", fun = "hidden_units"),
    has_submodel = FALSE
  )

  set_model_arg(
    model = "mlp",
    eng = "h2o",
    parsnip = "epochs",
    original = "epochs",
    func = list(pkg = "dials", fun = "epochs"),
    has_submodel = FALSE
  )

  set_model_arg(
    model = "mlp",
    eng = "h2o",
    parsnip = "activation",
    original = "activation",
    func = list(pkg = "dials", fun = "activation"),
    has_submodel = FALSE
  )


  set_fit(
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

  set_fit(
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

  set_pred(
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

  set_pred(
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

  set_pred(
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

  set_pred(
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

  set_pred(
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


#' Parsnip model specification
#'
#' @param formula formula
#' @param data data.frame of training data
#' @param l2 numeric, l2 regulation parameter, default = 0
#' @param hidden_dropout_ratios dropout ratio for single hidden layer, default
#' = 0
#' @param hidden integer, number of neurons in the hidden layer, default = 200
#' @param epochs integer, number of epochs, default = 10
#' @param activation character, activation function, default is
#' "RectifierWithDropout"
#' @param ... other arguments currently unused
#'
#' @return evaluated h2o model call
#' @export
#'
#' @importFrom rsample form_pred
#' @importFrom rlang call_modify eval_tidy current_env call2
#' @importFrom h2o h2o.init as.h2o
#' @importFrom stats terms
h2o_train <- function(formula, data, l2 = 0, hidden_dropout_ratios = 0,
                      hidden = 200, epochs = 10,
                      activation = "RectifierWithDropout", ...) {

  # "Tanh", classif
  # "TanhWithDropout", classif
  # "Rectifier", classif
  # "RectifierWithDropout", classif
  # "Maxout",
  # "MaxoutWithDropout"

  # define H2OFrame
  X <- form_pred(terms(formula, data = data))
  y <- setdiff(all.vars(terms(formula, data = data)), X)
  data <- as.h2o(data)

  if (hidden_dropout_ratios > 0) {
  }

  # define arguments
  args <- list(
    x = X,
    y = y,
    training_frame = data,
    l2 = l2,
    hidden_dropout_ratios = hidden_dropout_ratios,
    hidden = hidden,
    epochs = epochs,
    activation = activation
  )

  others <- list(...)

  # create unevaluated model call
  call <- parsnip:::make_call(fun = "h2o.deeplearning", ns = "h2o", args)

  if (length(others) > 0) {
    call <- call_modify(call, !!!others)
  }

  eval_tidy(call, env = current_env())
}


#' Wrapper for prediction for h2o mlp model
#'
#' @param object model
#' @param newdata data to predict
#' @param ... currently unused
#'
#' @return tibble
#' @export
#' @importFrom h2o h2o.init as.h2o
#' @importFrom stats predict
#' @importFrom tibble as_tibble
h2o_pred <- function(object, newdata, ...) {

  # convert to H2OFrame
  if (!inherits(newdata, "H2OFrame")) {
    newdata <- as.h2o(newdata)
  }

  res <- predict(object, newdata, ...)
  as_tibble(res)
}
