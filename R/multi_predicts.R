multi_predict._H2OMultinomialModel <-
  function(object, new_data, type = "class", ...) {

    args <- list(...)

    if (any(names(rlang::enquos(...)) == "newdata"))
      rlang::abort("Did you mean to use `new_data` instead of `newdata`?")

    res <- switch(
      object$fit@algorithm,
      gbm = gbm_multi_predict(object, new_data, type, args$trees)
    )

    res
  }


multi_predict._H2ORegressionModel <-
  function(object, new_data, type = "numeric", ...) {

    args <- list(...)

    if (any(names(rlang::enquos(...)) == "newdata"))
      rlang::abort("Did you mean to use `new_data` instead of `newdata`?")

    res <- switch(
      object$fit@algorithm,
      gbm = gbm_multi_predict(object, new_data, type, args$trees)
    )

    res
  }


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
