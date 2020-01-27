#' Extract response and predictor terms from a formula and a data.frame
#'
#' @param formula formula
#' @param data data.frame or tibble
#'
#' @return list, X = character vector of predictor names, y = character vector
#' of response variable
#' @export
#'
#' @examples
#' data(iris)
#' vars <- extract_terms(Species ~ ., iris)
extract_terms <- function(formula, data) {
  # check that formula is two-sided
  if (!attr(stats::terms(formula, data = data), "response"))
    stop("Two-sided formula is required")

  # extract terms
  X <- attr(stats::terms(formula, data = data), "term.labels")
  y <- all.vars(formula)[1]

  list(X = X, y = y)
}

