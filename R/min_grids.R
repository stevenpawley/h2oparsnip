# min grid for mlp -------------------------------------------------------------
submodel_mlp <- function (grid) {
  if (nrow(grid) == 1) {
    grid$.submodels <- list(list())
    return(grid)
  }

  nm <- colnames(grid)[1]
  fit_only <- tibble(nm = min(grid[[nm]], na.rm = TRUE))
  names(fit_only) <- nm
  sub_mods <- list(grid[[nm]][-which.min(grid[[nm]])])
  names(sub_mods) <- nm
  fit_only$.submodels <- list(sub_mods)
  dplyr::select(fit_only, dplyr::one_of(names(grid)), .submodels)
}


submodel_and_fixed_mlp <- function (grid, fixed_args) {
  orig_names <- names(grid)
  subm_nm <- orig_names[!(orig_names %in% fixed_args)]
  grid <- grid %>% dplyr::rename(..val = !!subm_nm)

  fit_only <-
    grid %>% dplyr::group_by(!!!rlang::syms(fixed_args)) %>%
    dplyr::summarize(min_val = min(..val, na.rm = TRUE)) %>%
    dplyr::ungroup()

  min_grid_df <-
    dplyr::full_join(fit_only, grid, by = fixed_args) %>%
    dplyr::filter(..val != min_val) %>% dplyr::group_by(!!!rlang::syms(fixed_args)) %>%
    dplyr::summarize(.submodels = list(lst(`:=`(!!subm_nm, ..val)))) %>%
    dplyr::ungroup() %>% dplyr::full_join(fit_only, by = fixed_args) %>%
    dplyr::rename(`:=`(!!subm_nm, min_val))

  dplyr::select(min_grid_df, dplyr::one_of(orig_names), .submodels) %>%
    dplyr::mutate_if(is.factor, as.character)
}


fit_min_value <- function(x, grid, ...) {
  gr_nms <- names(grid)
  param_info <- tune:::get_submodel_info(x)
  sub_nm <- param_info$id[param_info$has_submodel]

  if (length(sub_nm) == 0 | !any(names(grid) %in% sub_nm)) {
    return(blank_submodels(grid))
  }

  fixed_args <- gr_nms[gr_nms != sub_nm]

  if (length(fixed_args) == 0) {
    res <- submodel_mlp(grid)
  } else {
    res <- submodel_and_fixed_mlp(grid, fixed_args)
  }
  res
}

min_grid.mlp <- fit_min_value
