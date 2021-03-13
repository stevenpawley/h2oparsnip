#' Control aspects of the grid search process
#'
#' `control_h2o` provides a function to set various aspects of the grid search process.
#' By default during tuning, the resampling predictions are stored within the h2o
#' cluster. To save memory and space, use 'save_pred = FALSE'.
#' ``
#'
#' @param verbose A logical for logging results as they are generated.
#' @param save_pred A logical for whether the out-of-sample predictions should be saved
#'   for each model evaluated.
#' @param event_level A single string containing either "first" or "second". This
#'   argument is passed on to yardstick metric functions when any type of class
#'   prediction is made, and specifies which level of the outcome is considered the
#'   "event".
#'
#' @return An object of `control_grid` and `control_resamples` class.
#' @export
#'
#' @examples
#' # to save space in the cluster use these settings (the defaults)
#' control_h2o(verbose = TRUE, save_pred = FALSE)
control_h2o <- function(verbose = FALSE, save_pred = FALSE, save_workflow = FALSE,
                        event_level = "first") {

  res <- list(
    verbose = verbose,
    save_pred = save_pred,
    event_level = event_level
  )

  class(res) <- c("control_grid", "control_resamples")
  res
}
