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

#' @importFrom yardstick rsq rmse accuracy mn_log_loss pr_auc roc_auc
convert_h2o_metrics <- function(metrics) {

  allowed_metrics <- c(
    # regression
    "yardstick::rsq",
    "yardstick::rmse",
    "h2oparsnip::mse",

    # classification
    "yardstick::accuracy",
    "yardstick::mn_log_loss",
    "yardstick::pr_auc",
    "yardstick::roc_auc"
  )

  allowed_metrics <-
    c(allowed_metrics, gsub("yardstick::", "", allowed_metrics))
  allowed_metrics <-
    c(allowed_metrics, gsub("h2oparsnip::", "", allowed_metrics))

  if (any(!names(attributes(metrics)$metrics) %in% allowed_metrics)) {
    msg <- "`metrics` argument must contain a `yardstick::metric_set` with one or
      several of the following metrics:"
    rlang::abort(paste(msg, paste(allowed_metrics, collapse = ", ")))
  }

  metric_names <- names(attributes(metrics)$metric)
  metric_names <- gsub("yardstick::", "", metric_names)
  metric_names <- gsub("h2oparsnip::", "", metric_names)

  convert_metric <- function(yardstick_name) {
    switch(
      yardstick_name,
      # regression
      rsq = "r2",
      rmse = "rmse",
      mse = "mse",

      # classification
      accuracy = "accuracy",
      mn_log_loss = "logloss",
      roc_auc = "auc",
      pr_auc = "aucpr"
    )
  }

  sapply(metric_names, convert_metric)
}
