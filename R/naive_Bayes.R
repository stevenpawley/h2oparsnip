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
      defaults = list(
        model_id = paste("naive_bayes", as.integer(runif(1, 0, 1e9)), sep = "_")
      )
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
#' @param model_id A randomly assigned identifier for the model. Used to refer
#'   to the model within the h2o cluster.
#' @param laplace numeric, the Laplace smoothing parameter, must be >= 0.
#' @param ... other arguments not currently used
#'
#' @return evaluated h2o model call
#' @export
h2o_naiveBayes_train <-
  function(formula,
           data,
           model_id,
           laplace = 0,
           ...) {

    others <- list(...)

    # convert to H2OFrame, get response and predictor names
    dest_frame <- paste("training_data", model_id, sep = "_")
    pre <- preprocess_training(formula, data, dest_frame)

    # check arguments
    if (laplace < 0)
      laplace <- 0

    # define arguments
    args <- list(
      model_id = model_id,
      x = pre$X,
      y = pre$y,
      training_frame = pre$data,
      laplace = laplace
    )

    res <- make_h2o_call("h2o.naiveBayes", args, others)
    h2o::h2o.rm(dest_frame)

    res
  }
