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

    if (any(names(enquos(...)) == "newdata"))
      rlang::abort("Did you mean to use `new_data` instead of `newdata`?")

    res <- switch(
      object$fit@algorithm,
      gbm = gbm_multi_predict(object, new_data, type, args$trees),
      deeplearning = mlp_multi_predict(object, new_data, type, args$epochs),
      glm = glm_multi_predict(object, new_data, type, args$penalty)
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

    if (any(names(enquos(...)) == "newdata"))
      rlang::abort("Did you mean to use `new_data` instead of `newdata`?")

    res <- switch(
      object$fit@algorithm,
      gbm = gbm_multi_predict(object, new_data, type, args$trees),
      deeplearning = mlp_multi_predict(object, new_data, type, args$epochs),
      glm = glm_multi_predict(object, new_data, type, args$penalty)
    )

    res
  }

# ------------------------------------------------------------------------------
mlp_multi_predict <- function(object, new_data, type, epochs) {

  epochs <- sort(epochs)
  new_data <- h2o::as.h2o(new_data)

  preds <- map(epochs, function(epoch) {
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

    predict(model, new_data) %>% as.data.frame()
  })

  # prepare predictions in parsnip format
  if (type == "class") {
    res <- map2(
      preds, epochs, ~ select(.x, predict) %>%
        rename(.pred_class = predict) %>%
        mutate(
          epochs = .y,
          .pred_class = factor(.pred_class, levels = object$lvl),
          .row = 1:max(row_number())
        )
    )

  } else if (type == "prob") {
    new_names <- object$lvl
    names(new_names) <- paste(".pred", object$lvl, sep = "_")

    res <- map2(
      preds, epochs, ~ select(.x, !!!object$lvl) %>%
        rename(!!!new_names) %>%
        mutate(
          epochs = .y,
          .row = 1:max(row_number())
        )
    )

  } else if (type == "numeric") {
    res <- map2(preds, epochs, function(x, epoch)
      tibble(epochs = epoch, .pred = x$predict))
    res <- map(res, ~ mutate(.x, .row = 1:max(row_number())))
  }


  res <- bind_rows(res)
  res <- arrange(res, .row, epochs)
  res <- split(res[, -ncol(res)], res$.row)

  tibble(.pred = res)
}

# ------------------------------------------------------------------------------
gbm_multi_predict <- function(object, new_data, type, trees) {

  trees <- sort(trees)

  preds <- h2o::staged_predict_proba.H2OModel(
    object = object$fit,
    newdata = h2o::as.h2o(new_data)
  )
  preds <- as.data.frame(preds)

  if (type %in% c("class", "prob")) {
    n_classes <- length(object$lvl)

    res <- map(trees, function(tree) {
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
    res <- map(trees, function(tree) tibble(trees = tree, .pred = preds[, tree]))
  }

  # prepare predictions in parsnip format
  if (type == "class") {
    res <- map(
      res, ~ select(.x, trees, predict) %>%
        rename(.pred_class = predict) %>%
        mutate(
          .pred_class = factor(.pred_class, levels = object$lvl),
          .row = 1:max(row_number())
        )
    )

  } else if (type == "numeric") {
    res <- map(res, ~ mutate(.x, .row = 1:max(row_number())))

  } else if (type == "prob") {
    new_names <- object$lvl
    names(new_names) <- paste(".pred", object$lvl, sep = "_")
    res <- map(res, ~ select(.x, trees,!!!object$lvl) %>% rename(!!new_names)) %>%
      mutate(.row = 1:max(row_number()))
  }

  res <- bind_rows(res)
  res <- arrange(res, .row, trees)
  res <- split(res[, -ncol(res)], res$.row)

  tibble(.pred = res)
}

# glm --------------------------------------------------------------------------
# glm_multi_predict <- function(object, new_data, type, penalty) {
#
#   penalty <- sort(penalty)
#
#   # create basic call
#   model_args <- list(
#     model_id = paste("tune_glm", as.integer(runif(1, 0, 1e9)), sep = "_"),
#     family = object$fit@parameters$family,
#     training_frame = object$fit@parameters$training_frame,
#     x = object$fit@parameters$x,
#     y = object$fit@parameters$y,
#     alpha = object$fit@parameters$alpha
#   )
#   new_data <- h2o::as.h2o(new_data)
#
#   model_call <- rlang::call2(.fn = "h2o.glm", .ns = "h2o", !!!model_args)
#
#   # fit models for each hyperparameter
#   preds <- map(penalty, function(lambda) {
#     updated_call <- rlang::call_modify(model_call, lambda = lambda)
#     retrained_model <- rlang::eval_tidy(updated_call)
#     as.data.frame(predict(retrained_model, new_data))
#     h2o::h2o.rm(model_args$model_id)
#   })
#
#   # prepare predictions in parsnip format
#   if (type == "class") {
#     res <- map2(
#       preds, penalty, ~ select(.x, predict) %>%
#         rename(.pred_class = predict) %>%
#         mutate(
#           penalty = .y,
#           .pred_class = factor(.pred_class, levels = object$lvl),
#           .row = 1:max(row_number())
#         )
#     )
#
#   } else if (type == "prob") {
#     new_names <- object$lvl
#     names(new_names) <- paste(".pred", object$lvl, sep = "_")
#
#     res <- map2(
#       preds, penalty, ~ select(.x, !!!object$lvl) %>%
#         rename(!!!new_names) %>%
#         mutate(
#           penalty = .y,
#           .row = 1:max(row_number())
#         )
#     )
#
#   } else if (type == "numeric") {
#     res <- map2(preds, penalty, function(x, p)
#       tibble(penalty = p, .pred = x$predict))
#     res <- map(res, ~ mutate(.x, .row = 1:max(row_number())))
#   }
#
#   res <- bind_rows(res)
#   res <- arrange(res, .row, penalty)
#   res <- split(res[, -ncol(res)], res$.row)
#
#   tibble(.pred = res)
# }


# glm_multi_predict <- function(object, new_data, type, args) {
#
#   # translate arguments
#   translate_names <- c(
#     penalty = "lambda",
#     mixture = "alpha"
#   )
#
#   args <- map(args, sort)
#
#   rename_args <-
#     sapply(names(args), function(nm)
#       if (nm %in% names(translate_names))
#         translate_names[names(translate_names) == nm] else nm,
#       USE.NAMES = FALSE
#     )
#   names(args) <- rename_args
#
#   # create grid
#   param_grid <- do.call(expand.grid, args)
#
#   # create basic call
#   model_args <- list(
#     model_id = object$fit@model_id,
#     family = object$spec$defaults$family, # object$fit@parameters$family
#     training_frame = object$fit@parameters$training_frame,
#     x = object$fit@parameters$x,
#     y = object$fit@parameters$y
#   )
#   new_data <- h2o::as.h2o(new_data)
#
#   model_call <- rlang::call2(.fn = "h2o.glm", .ns = "h2o", !!!model_args)
#
#   # fit models for each hyperparameter
#   preds <- pmap(param_grid, function(...) {
#     hyperparams <- list(...)
#     updated_call <- rlang::call_modify(model_call, !!!hyperparams)
#     retrained_model <- rlang::eval_tidy(updated_call)
#     as.data.frame(predict(retrained_model, new_data))
#   })
#
#   # convert param grid to list
#   param_lst <- pmap(param_grid, function(...) list(...))
#
#   # prepare predictions in parsnip format
#   if (type == "class") {
#     res <- map2(
#       preds, epochs, ~ select(.x, predict) %>%
#         rename(.pred_class = predict) %>%
#         mutate(
#           epochs = .y,
#           .pred_class = factor(.pred_class, levels = object$lvl),
#           .row = 1:max(row_number())
#         )
#     )
#
#   } else if (type == "prob") {
#     new_names <- object$lvl
#     names(new_names) <- paste(".pred", object$lvl, sep = "_")
#
#     res <- map2(
#       preds, epochs, ~ select(.x, !!!object$lvl) %>%
#         rename(!!!new_names) %>%
#         mutate(
#           epochs = .y,
#           .row = 1:max(row_number())
#         )
#     )
#
#   } else if (type == "numeric") {
#     res <- map2(preds, param_lst, function(x, params) {
#      df = as_tibble(params)
#      x <- tibble(.pred = x$predict)
#      as_tibble(cbind(df, x))
#     })
#     res <- map(res, ~ mutate(.x, .row = 1:max(row_number())))
#   }
#
#   res <- bind_rows(res)
#   arg_names <- syms(names(args))
#   res <- arrange(res, .row, !!!arg_names)
#   res <- split(res[, -ncol(res)], res$.row)
#
#   tibble(.pred = res)
# }
