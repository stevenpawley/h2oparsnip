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
