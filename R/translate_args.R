translate_args <- function(model_name) {

  envir <- get_model_env()

  args <-
    ls(envir) %>%
    tibble::tibble(name = .) %>%
    dplyr::filter(grepl("args", name)) %>%
    dplyr::mutate(model = sub("_args", "", name),
                  args  = purrr::map(name, ~envir[[.x]])) %>%
    tidyr::unnest(args) %>%
    dplyr::select(model:original)

  args %>%
    dplyr::filter(grepl(model_name, model)) %>%
    dplyr::select(-model) %>%
    tidyr::pivot_wider(names_from = engine, values_from = original)
}


rename_list <- function(x, new_names) {
  rename_args <-
    sapply(names(x), function(nm)
      if (nm %in% names(new_names))
        new_names[names(new_names) == nm] else nm,
      USE.NAMES = FALSE
    )
  names(x) <- rename_args
  x
}
