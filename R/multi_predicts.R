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
  function(object, new_data, type = NULL, ...) {

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
  function(object, new_data, type = NULL, ...) {

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


#' Multi_predict method for glm
#'
#' @param object A parsnip object that inherits from `model_fit` and
#'   `_H2OMultinomialModel`.
#' @param new_data A data.frame of new data used for the prediction.
#' @param type A character string of either "class", "prob" or "numeric"
#' @param penalty A numeric vector with the penalty values for different submodels.
#'
#' @return A [tibble][tibble::tibble-package].
glm_multi_predict <- function(object, new_data, type, penalty) {
  penalty <- sort(penalty)

  # create basic call
  model_args <- list(
    model_id = paste("tune_glm", as.integer(runif(1, 0, 1e9)), sep = "_"),
    family = object$fit@parameters$family,
    training_frame = object$fit@parameters$training_frame,
    x = object$fit@parameters$x,
    y = object$fit@parameters$y,
    alpha = object$fit@parameters$alpha
  )
  new_data <- h2o::as.h2o(new_data)

  model_call <- rlang::call2(.fn = "h2o.glm", .ns = "h2o", !!!model_args)

  # fit models for each hyperparameter
  preds <- map(penalty, function(lambda) {
    updated_call <- rlang::call_modify(model_call, lambda = lambda)
    retrained_model <- rlang::eval_tidy(updated_call)
    as.data.frame(predict(retrained_model, new_data))
    h2o::h2o.rm(model_args$model_id)
  })

  # prepare predictions in parsnip format
  if (type == "class") {
    res <- purrr::map2(
      preds,
      penalty,
      ~ dplyr::select(.x, predict) %>%
        dplyr::rename(.pred_class = predict) %>%
        dplyr::mutate(
          penalty = .y,
          .pred_class = factor(.pred_class, levels = object$lvl),
          .row = 1:max(dplyr::row_number())
        )
    )

  } else if (type == "prob") {
    new_names <- object$lvl
    names(new_names) <- paste(".pred", object$lvl, sep = "_")

    res <- purrr::map2(
      preds,
      penalty,
      ~ dplyr::select(.x,!!!object$lvl) %>%
        dplyr::rename(!!!new_names) %>%
        dplyr::mutate(penalty = .y,
                      .row = 1:max(dplyr::row_number()))
    )

  } else if (type == "numeric") {
    res <- purrr::map2(preds, penalty, function(x, p)
      tibble(penalty = p, .pred = x$predict))

    res <- purrr::map(res, ~ mutate(.x, .row = 1:max(dplyr::row_number())))
  }

  res <- dplyr::bind_rows(res)
  res <- dplyr::arrange(res, .row, penalty)
  res <- split(res[, -ncol(res)], res$.row)

  tibble(.pred = res)
}
