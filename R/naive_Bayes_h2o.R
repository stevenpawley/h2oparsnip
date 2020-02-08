#' Wrapper to add the `h2o` engine to the discrim `naiveBayes` model specification
#'
#' @return NULL
#' @export
add_naive_Bayes_h2o <- function() {

  parsnip::set_model_engine("naive_Bayes", "classification", "h2o")
  parsnip::set_dependency("naive_Bayes", "h2o", "h2o")

  parsnip::set_model_arg(
    model = "naive_Bayes",
    eng = "h2o",
    parsnip = "Laplace ",
    original = "laplace",
    func = list(pkg = "dials", fun = "dropout"),
    has_submodel = FALSE
  )

  # fit
  parsnip::set_fit(
    model = "naive_Bayes",
    eng = "h2o",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "x", "y", "training_frame"),
      func = c(fun = "h2o_naiveBayes_train"),
      defaults = list()
    )
  )

  # classification predict
  parsnip::set_pred(
    model = "naive_Bayes",
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
    model = "naive_Bayes",
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
    model = "naive_Bayes",
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

#' Wrapper for training a h2o.naiveBayes model as part of a discrim `naive_Bayes`
#' h2o engine
#'
#' @param formula formula
#' @param data data.frame of training data
#' @param laplace numeric, the Laplace smoothing parameter, must be >= 0.
#' @param ... other arguments not currently used
#'
#' @return evaluated h2o model call
#' @export
h2o_naiveBayes_train <-
  function(formula,
           data,
           laplace = 0,
           ...) {

    # convert to H2OFrame, get response and predictor names
    pre <- preprocess_training(formula, data)

    # check arguments
    if (laplace < 0)
      laplace <- 0

    # define arguments
    args <- list(
      x = pre$X,
      y = pre$y,
      training_frame = pre$data,
      laplace = laplace
    )

    others <- list(...)
    make_h2o_call("h2o.naiveBayes", args, others)
  }
