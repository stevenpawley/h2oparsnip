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


#' Preprocess training data for use with h2o models
#'
#' @param formula formula object.
#' @param data data.frame of predictors and response variable.
#' @param dest_frame An identifier for the H2OFrame.
#'
#' @return list(data = H2OFrame, X = predictor names, y = response name).
#' @export
preprocess_training <- function(formula, data, dest_frame) {

  X <- extract_terms(formula, data)$X
  y <- extract_terms(formula, data)$y

  if (!inherits(data, "H2OFrame"))
    data <- h2o::as.h2o(data, dest_frame)

  list(
    data = data,
    X = X,
    y = y
  )
}


#' Create call to a h2o model by merging arguments
#'
#' @param .fn character, name of h2o model e.g. h2o.randomForest
#' @param args list of arguments to include within call to model
#' @param others list of additional args passed through ...
#'
#' @return h2o trained model
#' @export
make_h2o_call <- function(.fn, args, others) {

  # remove args with NULLs
  args <- args[lapply(args, length) > 0]

  # create unevaluated model call
  model_call <- rlang::call2(.fn = .fn, !!!args, .ns = "h2o")

  # add others if not NULL
  if (length(others) > 0)
    model_call <- rlang::call_modify(model_call, !!!others)

  rlang::eval_tidy(model_call)
}
