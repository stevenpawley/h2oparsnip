translate_args <- function(model_name) {
  envir <- parsnip::get_model_env()

  args <- tibble::tibble(ls(envir))
  args <- rlang::set_names(args, "name")
  args <- args[grepl("args", args$name), ]

  args$model <- sub("_args", "", args$name)
  args$args <- lapply(args$name, function(x) envir[[x]])

  args <- args %>%
    tidyr::unnest("args") %>%
    dplyr::select(!!rlang::sym("model"):!!rlang::sym("original"))

  args <- args[args$model == model_name, ]

  args %>%
    dplyr::select(-dplyr::one_of("model")) %>%
    tidyr::pivot_wider(
      names_from = !!rlang::sym("engine"),
      values_from = !!rlang::sym("original")
    )
}

rename_list <- function(x, new_names) {
  rename_args <-
    sapply(names(x), function(nm) {
      if (nm %in% names(new_names)) {
        new_names[names(new_names) == nm]
      } else {
        nm
      }
    },
    USE.NAMES = FALSE
    )
  names(x) <- rename_args
  x
}
