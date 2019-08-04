
# json ----
get_cols_sep <-
  function(data, col, sep, validate = TRUE) {
    
    # Probably should always validate, but this is also done in `separate_cols_max()`,
    # so don't need to validate here if called from `separate_cols_max()`
    # since this validation is already done there.
    if(validate) {
      stopifnot(is.data.frame(data))
      stopifnot(is.character(col))
      nms <- data %>% colnames()
      stopifnot(any(col %in% nms))
      col_sym <- sym(col)
      x <- dplyr::pull(data, !!col_sym)
    } else {
      x <- data[[col]]
    }
    x <- stringr::str_split(x, sep)
    x <- purrr::map_int(x, length)
    max(x)
  }

separate_cols_max <-
  function(data, col = 'name', sep = '\\.', n_cols_sep = NULL, ..., fill = 'right') {
    
    # Note that `fill` will be validated by `tidyr::separate()`.
    stopifnot(
      is.data.frame(data),
      is.character(col),
      is.character(sep)
    )
    nms <- data %>% colnames()
    stopifnot(any(col %in% nms))
    col_sym <- sym(col)
    
    if(is.null(n_cols_sep)) {
      n_cols_sep <-
        get_cols_sep(data = data, col = col, sep = sep, validate = FALSE)
    }
    stopifnot(is.integer(n_cols_sep))
    nms_sep <-
      paste0(col, seq(1L, n_cols_sep, by = 1L))
    tidyr::separate(data, col = !!col_sym, into = nms_sep, sep = sep, fill = fill, ...)
  }

convert_list_to_tbl <-
  function(x, name = 'name', value = 'value') {
    stopifnot(is.list(x))
    tibble::enframe(x = unlist(x), name = name, value = value)
  }

unlist_tidily <-
  function(x, ..., name = 'name', value = 'value') {
    data <- convert_list_to_tbl(x = x, name = name, value = value)
    separate_cols_max(data, col = name, ...)
  }