#' Wrapper for prediction for h2o mlp model
#'
#' @param object H2O model
#' @param newdata data to predict
#' @param ... currently unused
#'
#' @return tibble
#' @export
h2o_pred <- function(object, newdata, ...) {

  # convert to H2OFrame
  if (!inherits(newdata, "H2OFrame"))
    newdata <- h2o::as.h2o(newdata)

  res <- stats::predict(object, newdata, ...)
  tibble::as_tibble(res)
}
