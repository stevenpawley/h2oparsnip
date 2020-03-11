#' General interface for automl models
#'
#' @param mode A single character string for the type of model.
#'
#' @return A model_spec
#' @export
automl <- function(mode = "classification") {
  args <- list()

  new_model_spec(
    "automl",
    args = args,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = "h2o"
  )
}

#' @export
print.automl <- function(x, ...) {
  cat("Automl Model Specification (", x$mode, ")\n\n", sep = "")
  parsnip::model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(parsnip::show_call(x))
  }

  invisible(x)
}


