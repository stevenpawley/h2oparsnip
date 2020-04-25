#' Multi_predict method for h2o classification models
#'
#' @param object A `model_spec` object.
#' @param new_data A data.frame of new observations.
#' @param type A single character vector of "class" or "prob".
#' @param ... Other arguments.
#'
#' @return
#' @export
multi_predict._H2OMultinomialModel <-
  function(object, new_data, type = "class", ...) {

    args <- list(...)

    if (any(names(enquos(...)) == "newdata"))
      rlang::abort("Did you mean to use `new_data` instead of `newdata`?")

    res <- switch(
      object$fit@algorithm,
      gbm = gbm_multi_predict(object, new_data, type, args$trees),
      deeplearning = mlp_multi_predict(object, new_data, type, args$epochs)
    )

    res
  }


#' Multi_predict method for h2o regression models
#'
#' @param object A `model_spec` object.
#' @param new_data A data.frame of new observations.
#' @param type A single character vector, must be "numeric".
#' @param ... Other arguments.
#'
#' @return
#' @export
multi_predict._H2ORegressionModel <-
  function(object, new_data, type = "numeric", ...) {

    args <- list(...)

    if (any(names(enquos(...)) == "newdata"))
      rlang::abort("Did you mean to use `new_data` instead of `newdata`?")

    res <- switch(
      object$fit@algorithm,
      gbm = gbm_multi_predict(object, new_data, type, args$trees),
      deeplearning = mlp_multi_predict(object, new_data, type, args$epochs)
    )

    res
  }
