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

    if (any(names(rlang::enquos(...)) == "newdata"))
      rlang::abort("Did you mean to use `new_data` instead of `newdata`?")

    res <- switch(
      object$fit@algorithm,
      gbm = gbm_multi_predict(object, new_data, type, args$trees),
      deeplearning = mlp_multi_predict(object, new_data, type, args$epochs)
      # glm = glm_multi_predict(object, new_data, type, args$penalty)
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

    if (any(names(rlang::enquos(...)) == "newdata"))
      rlang::abort("Did you mean to use `new_data` instead of `newdata`?")

    res <- switch(
      object$fit@algorithm,
      gbm = gbm_multi_predict(object, new_data, type, args$trees),
      deeplearning = mlp_multi_predict(object, new_data, type, args$epochs)
      # glm = glm_multi_predict(object, new_data, type, args$penalty)
    )

    res
  }


#' Multi_predict method for mlp
#'
#' @param object A parsnip object that inherits from `model_fit` and
#'   `_H2OMultinomialModel`.
#' @param new_data A data.frame of new data used for the prediction.
#' @param type A character string of either "class", "prob" or "numeric"
#' @param epochs An integer vector with the number of epochs to produce
#'   predictions for.
#'
#' @return A [tibble][tibble::tibble-package].
mlp_multi_predict <- function(object, new_data, type, epochs) {

  epochs <- sort(epochs)
  new_data <- h2o::as.h2o(new_data)

  preds <- purrr::map(epochs, function(epoch) {
    model <- h2o::h2o.deeplearning(
      x = object$fit@parameters$x,
      y = object$fit@parameters$y,
      training_frame = object$fit@parameters$training_frame,
      checkpoint = object$fit@model_id,
      epochs = epoch,
      hidden = object$fit@parameters$hidden,
      l2 = object$fit@parameters$l2,
      hidden_dropout_ratios = object$fit@parameters$hidden_dropout_ratios,
      activation = object$fit@parameters$activation
    )

    predict(model, new_data) %>%
      as.data.frame()
  })

  # prepare predictions in parsnip format
  if (type == "class") {
    res <- purrr::map2(
      preds,
      epochs,
      ~ dplyr::select(.x, predict) %>%
        dplyr::rename(.pred_class = predict) %>%
        dplyr::mutate(
          epochs = .y,
          .pred_class = factor(.pred_class, levels = object$lvl),
          .row = 1:max(dplyr::row_number())
        )
    )

  } else if (type == "prob") {
    new_names <- object$lvl
    names(new_names) <- paste(".pred", object$lvl, sep = "_")

    res <- purrr::map2(
      preds,
      epochs,
      ~ dplyr::select(.x, !!!object$lvl) %>%
        dplyr::rename(!!!new_names) %>%
        dplyr::mutate(epochs = .y, .row = 1:max(dplyr::row_number()))
    )

  } else if (type == "numeric") {
    res <- purrr::map2(preds, epochs, function(x, epoch)
      tibble(epochs = epoch, .pred = x$predict))

    res <- purrr::map(res, ~ dplyr::mutate(.x, .row = 1:max(dplyr::row_number())))
  }

  res <- dplyr::bind_rows(res)
  res <- dplyr::arrange(res, .row, epochs)
  res <- split(res[, -ncol(res)], res$.row)

  tibble(.pred = res)
}


#' Multi_predict method for gbm
#'
#' @param object A parsnip object that inherits from `model_fit` and
#'   `_H2OMultinomialModel`.
#' @param new_data A data.frame of new data used for the prediction.
#' @param type A character string of either "class", "prob" or "numeric"
#' @param trees An integer vector with the number of trees for each submodel.
#'
#' @return A [tibble][tibble::tibble-package].
gbm_multi_predict <- function(object, new_data, type, trees) {

  trees <- sort(trees)

  preds <- h2o::staged_predict_proba.H2OModel(
    object = object$fit,
    newdata = h2o::as.h2o(new_data)
  )

  preds <- as.data.frame(preds)

  if (type %in% c("class", "prob")) {
    n_classes <- length(object$lvl)

    res <- purrr::map(trees, function(tree) {
      pred_class_names <- paste0("C", seq_len(n_classes))
      tree_names <- paste0("T", tree)
      x <- preds[, paste(tree_names, pred_class_names, sep = ".")]
      colnames(x) <- object$lvl
      x <- as.data.frame(x)
      x$predict <- object$lvl[apply(x, 1, which.max)]
      x$trees <- tree
      x
    })

  } else if (type == "numeric") {
    res <- purrr::map(trees, function(tree)
      tibble(trees = tree, .pred = preds[, tree]))
  }

  # prepare predictions in parsnip format
  if (type == "class") {
    res <- purrr::map(
      res,
      ~ dplyr::select(.x, trees, predict) %>%
        dplyr::rename(.pred_class = predict) %>%
        dplyr::mutate(
          .pred_class = factor(.pred_class, levels = object$lvl),
          .row = 1:max(dplyr::row_number())
        )
    )

  } else if (type == "numeric") {
    res <- purrr::map(
      res,
      ~ dplyr::mutate(.x, .row = 1:max(dplyr::row_number()))
    )

  } else if (type == "prob") {
    new_names <- object$lvl
    names(new_names) <- paste(".pred", object$lvl, sep = "_")

    res <- purrr::map(
      res,
      ~ dplyr::select(.x, trees, !!!object$lvl) %>%
        dplyr::rename(!!new_names)
    )

    res <- res %>%
      dplyr::mutate(.row = 1:max(dplyr::row_number()))
  }

  res <- dplyr::bind_rows(res)
  res <- dplyr::arrange(res, .row, trees)
  res <- split(res[, -ncol(res)], res$.row)

  tibble(.pred = res)
}
