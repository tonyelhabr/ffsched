
clean_json <- function(data, ...) {
  data %>% 
    unlist_tidily() %>% 
    mutate(idx = row_number()) %>% 
    select(idx, everything())
}

drop_na_cols <- function(data, ...) {
  data %>% select_if(~!all(is.na(.)))
}