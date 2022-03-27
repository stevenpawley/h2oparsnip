#' Saves an H2O model to file that is contained within a fitted parsnip model
#' specification or contained within a workflow
#'
#' H2O models cannot be saved using the typical R approaches such as saveRDS
#' because the actual H2O model is contained within a Java virtual machine. H2O
#' models need to be saved and restored using the `h2o::h2o.saveModel` and
#' `h2o::h2o.loadModel` functions. This is inconvenient for using H2O models
#' contained within parsnip model specifications or workflow objects.
#'
#' The `write_h2o` function extracts the H2O model from within a parsnip or
#' workflow fitted model and saves it to file using the `h2o::h2o.saveModel`
#' function. To restore a model and insert it back into a previously fitted
#' model use the `read_h2o` function.
#'
#' @param object Either a `workflows::workflow()` object contained a fitted
#'   model when using the workflows package, or a `model_spec` object from a
#'   fitted model when using the parsnip package directly.
#' @param filename A `character` specifying the file path used to save the
#'   model. H2O models do not require a specific file extension.
#' @param ... Currently not used.
#'
#' @return The file path used to save the model.
#' @export
#'
#' @examples
#' library(parsnip)
#' library(h2o)
#'
#' # start a h2o session
#' h2o.init()
#'
#' # fit a parsnip model using the h2o engine
#' clf <- mlp(mode = "classification") %>%
#'   set_engine("h2o")
#'
#' model_fit <- clf %>% fit(Species ~ ., iris)
#'
#' # save the parsnip model
#' saveRDS(model_fit, file.path(tempdir(), "my_model.rds"))
#'
#' # save the h2o component of the model
#' write_h2o(object = model_fit, filename = file.path(tempdir(), "my_h2o_model.mod"))
#' h2o.shutdown(prompt = FALSE)
#'
#' # restore a model
#' h2o.init()
#' model_fit <- readRDS(file.path(tempdir(), "my_model.rds"))
#'
#' # read and insert the H2O model back into the parsnip object
#' model_fit <- read_h2o(model_fit, file.path(tempdir(), "my_h2o_model.mod"))
write_h2o <- function(object, filename, ...) {
  UseMethod("write_h2o", object)
}


#' @export
#' @rdname write_h2o
read_h2o <- function(object, filename, ...) {
  UseMethod("read_h2o", object)
}


#' @export
#' @rdname write_h2o
write_h2o.workflow <- function(object, filename, ...) {
  # extract model from workflow
  parsnip_model_spec <- workflows::pull_workflow_fit(object)

  # save H2O model which uses the model ID as the filename
  directory <- dirname(filename)
  fileout <- h2o::h2o.saveModel(
    object = parsnip_model_spec$fit,
    path = directory,
    force = TRUE
  )

  # rename the file
  file.rename(fileout, filename)

  filename
}


#' @export
#' @rdname write_h2o
write_h2o.model_fit <- function(object, filename, ...) {
  # extract the h2o model
  fit_obj <- object$fit

  # save H2O model which uses the model ID as the filename
  directory <- dirname(filename)
  fileout <- h2o::h2o.saveModel(
    object = fit_obj,
    path = directory,
    force = TRUE
  )

  # rename the file
  file.rename(fileout, filename)

  filename
}


#' @export
#' @rdname write_h2o
read_h2o.workflow <- function(object, filename, ...) {
  h2o_model <- h2o::h2o.loadModel(filename)
  object$fit$fit$fit <- h2o_model
  object
}


#' @export
#' @rdname write_h2o
read_h2o.model_fit <- function(object, filename, ...) {
  h2o_model <- h2o::h2o.loadModel(filename)
  object$fit <- h2o_model
  object
}
