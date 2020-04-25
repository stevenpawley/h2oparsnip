#' Wrapper for training a h2o.glm model as part of a parsnip
#'
#' @param formula formula
#' @param data data.frame of training data
#' @param model_id A randomly assigned identifier for the model. Used to refer
#'   to the model within the h2o cluster.
#' @param alpha numeric, Distribution of regularization between the L1 (Lasso)
#'   and L2 (Ridge) penalties. A value of 1 for alpha represents Lasso
#'   regression, a value of 0 produces Ridge regression.
#' @param lambda numeric, regularization strength
#' @param family character, one of c("gaussian", "binomial", "quasibinomial",
#'   "ordinal", "multinomial", "poisson", "gamma", "tweedie",
#'   "negativebinomial")
#' @param ... other arguments not currently used
#'
#' @return evaluated h2o model call
#' @export
h2o_glm_train <-
  function(formula,
           data,
           model_id,
           alpha = NULL,
           lambda = NULL,
           family = NULL,
           ...) {

    others <- list(...)

    # convert to H2OFrame, get response and predictor names
    dest_frame <- paste("training_data", model_id, sep = "_")
    pre <- preprocess_training(formula, data, dest_frame)

    # define arguments
    args <- list(
      model_id = model_id,
      x = pre$X,
      y = pre$y,
      training_frame = pre$data,
      alpha = alpha,
      lambda = lambda,
      family = family
    )

    res <- make_h2o_call("h2o.glm", args, others)
    h2o::h2o.rm(dest_frame)

    res
  }
