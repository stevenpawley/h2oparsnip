#' Mean squared error
#'
#' Calculate the mean squared error. This metric is in squared units of the original data.
#'
#' @param data A `data.frame` containing the `truth` and `estimate` columns.
#' @param truth The column identifier for the true class results (that is a
#'   factor). This should be an unquoted column name although this argument is
#'   passed by expression and supports quasiquotation (you can unquote column
#'   names). For _vec() functions, a factor vector.
#' @param estimate The column identifier for the predicted class results (that
#'   is also factor). As with truth this can be specified different ways but the
#'   primary method is to use an unquoted variable name. For _vec() functions, a
#'   factor vector.
#' @param na_rm A logical value indicating whether NA values should be stripped
#'   before the computation proceeds.
#' @param ... Not currently used.
#'
#' @return A `tibble` with columns `.metric`, `.estimator`, and `.estimate` and 1 row of values.
#' @export
mse_vec <- function(truth, estimate, na_rm = TRUE, ...) {
  mse_impl <- function(truth, estimate) {
    mean((estimate - truth)^2)
  }

  yardstick::metric_vec_template(
    metric_impl = mse_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
}

#' @export
#' @rdname mse_vec
mse <- function(data, ...) {
  UseMethod("mse")
}

class(mse) <- c("numeric_metric", "function")
attr(mse, "direction") <- "minimize"


#' @export
#' @rdname mse_vec
mse.data.frame <- function(data, truth, estimate, na_rm = TRUE, ...) {
  yardstick::metric_summarizer(
    metric_nm = "mse",
    metric_fn = mse_vec,
    data = data,
    truth = !! rlang::enquo(truth),
    estimate = !! rlang::enquo(estimate),
    na_rm = na_rm,
    ...
  )
}

#' Converts metrics that are applicable to h2o.getGrid into equivalent yardstick
#' `metric_set` objects.
#'
#' @param metric A character vector of the yardstick metric, one of c("rsq",
#'   "sensitivity", "rmse", "accuracy", "mn_log_loss", "mse")
#'
#' @return A list with `name` and `metric_set`
#' @importFrom yardstick rsq sensitivity rmse accuracy mn_log_loss
convert_h2o_metrics <- function(metric) {
  supported_metrics = c("rsq", "sensitivity", "rmse", "accuracy", "mn_log_loss", "mse")

  if (!metric %in% supported_metrics)
    rlang::abort(paste("metric must be one of the following supported metrics:", supported_metrics))

  yardstick_metric <- switch(
    metric,
    rsq = yardstick::metric_set(rsq),
    sensitivity = yardstick::metric_set(sensitivity),
    rmse = yardstick::metric_set(rmse),
    accuracy = yardstick::metric_set(accuracy),
    mn_log_loss = yardstick::metric_set(mn_log_loss),
    mse = yardstick::metric_set(mse)
  )

  h2o_name <- switch(
    metric,
    rsq = "r2",
    sensitivity = "max_per_class_error",
    rmse = "rmse",
    accuracy = "accuracy",
    mn_log_loss = "logloss",
    mse = "mse"

  )

  list(name = h2o_name, metric_set = yardstick_metric)
}
