add_naive_Bayes_h2o <- function() {
  parsnip::set_model_engine("naive_Bayes", "classification", "h2o")
  parsnip::set_dependency("naive_Bayes", "h2o", "h2o")

  parsnip::set_model_arg(
    model = "naive_Bayes",
    eng = "h2o",
    parsnip = "Laplace",
    original = "laplace",
    func = list(pkg = "dials", fun = "Laplace"),
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
  parsnip::set_encoding(
    model = "naive_Bayes",
    eng = "h2o",
    mode = "classification",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
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
#' @param ... other arguments passed to the h2o engine.
#'
#' @return a fitted h2o model.
#' @export
h2o_naiveBayes_train <-
  function(formula,
           data,
           laplace = 0,
           ...) {
    others <- list(...)

    # get term names and convert to h2o
    X <- attr(stats::terms(formula, data = data), "term.labels")
    y <- all.vars(formula)[1]

    # convert to H2OFrame (although parsnip doesn't support H2OFrames right now)
    if (!inherits(data, "H2OFrame")) {
      data <- h2o::as.h2o(data)
    }

    # check arguments
    if (laplace < 0) {
      laplace <- 0
    }

    # define arguments
    args <- list(
      x = X,
      y = y,
      training_frame = data,
      laplace = laplace
    )

    res <- make_h2o_call("h2o.naiveBayes", args, others)

    res
  }
