#' Wrapper for training a h2o.glm model as part of a parsnip
#'
#' @param formula formula
#' @param data data.frame of training data
#' @param alpha numeric, Distribution of regularization between the L1 (Lasso)
#'   and L2 (Ridge) penalties. A value of 1 for alpha represents Lasso
#'   regression, a value of 0 produces Ridge regression.
#' @param lambda numeric, regularization strength
#' @param family character, one of c("gaussian", "binomial", "quasibinomial",
#'   "ordinal", "multinomial", "poisson", "gamma", "tweedie",
#'   "negativebinomial")
#' @param ... other arguments passed to the h2o engine.
#'
#' @return evaluated h2o model call
#' @export
h2o_glm_train <-
  function(formula,
           data,
           alpha = NULL,
           lambda = NULL,
           family = NULL,
           ...) {
    others <- list(...)

    # get term names and convert to h2o
    X <- attr(stats::terms(formula, data = data), "term.labels")
    y <- all.vars(formula)[1]

    # convert to H2OFrame (although parsnip doesn't support H2OFrames right now)
    if (!inherits(data, "H2OFrame")) {
      data <- h2o::as.h2o(data)
    }

    # define arguments
    args <- list(
      x = X,
      y = y,
      training_frame = data,
      alpha = alpha,
      lambda = lambda,
      family = family
    )

    res <- make_h2o_call("h2o.glm", args, others)

    if (!"alpha" %in% names(res@parameters)) {
      res@parameters$alpha <- alpha
    }

    if (!"lambda" %in% names(res@parameters)) {
      res@parameters$lambda <- lambda
    }

    if (!"family" %in% names(res@parameters)) {
      res@parameters$family <- family
    }

    res
  }
