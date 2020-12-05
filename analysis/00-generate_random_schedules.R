

set.seed(42)
n_tm <- 10
n_wk <- n_tm - 1
idx_tm <- 1:n_tm
idx_wk <- 2:n_wk
# Random first week
.initialize <- function(seed = 42, n_retry = 10) {
  # set.seed(seed)
  mat <- matrix(nrow = n_tm, ncol = n_wk)
  tm_i <- 1
  wk_i <- 1
  tm1_wk1 <- sample(2:n_tm, 1)
  mat[tm_i, wk_i] <- tm1_wk1
  retry_i <- 1
  tm_i <- 2
  while(retry_i <= n_retry & tm_i <= n_tm) {
    tms_possible <- setdiff(idx_tm, tm_i)
    tms_already_indexed <- 1:(tm_i - 1)
    tms_already_matched <- mat[tms_already_indexed, wk_i]
    if(tm_i %in% tms_already_matched) {
      tmi_wki <- which(tm_i == tms_already_matched)
      mat[tm_i, wk_i] <- tmi_wki
      tm_i <- tm_i + 1
    } else {
      tms_cant_match <- unique(c(tms_already_indexed, tms_already_matched))
      tms_unmatched <- setdiff(tms_possible, tms_cant_match)
      n_matched <- length(tms_unmatched)
      if(n_matched == 0) {
        mat[2:n_tm, wk_i] <- NA
        tm_i <- 2
        cat('\n')
        cat(glue::glue('Retry {retry_i}.'), sep = '\n')
        cat(glue::glue('Re-trying `wk_i = {wk_i}`{paste0(rep("-", 62), collapse = "")}'), sep = '\n') 
      } else {
        tmi_wki <- if(n_matched == 1) {
          tms_unmatched
        } else {
          sample(tms_unmatched, 1)
        }
        # cat(glue::glue('`tmi_wki = {tmi_wki}`'), sep = '\n')
        mat[tm_i, wk_i] <- tmi_wki
        tm_i <- tm_i + 1
      }
    }
  }
  tms_possible <- setdiff(idx_tm, c(1, tm1_wk1))
  tm1_all_wks <- sample(tms_possible, size = n_wk - 1)
  
  mat[1, 2:n_wk] <- tm1_all_wks
  mat
}

# Iterate by week and team, always checking for other matched teams in the same week and previously played teams in prior weeks
generate <- function(seed = 1, n_retry = 100) {
  # set.seed(seed)
  mat <- .initialize()
  wk_i <- 2
  retry_i <- 1
  while(wk_i < n_wk) {
    # browser()
    # cat(paste0(rep('-', 80), sep = '', collapse = ''), sep = '\n')
    # cat(glue::glue('`wk_i = {wk_i}`{paste0(rep("-", 72), collapse = "")}'), sep = '\n')
    tm_i <- 2
    while(retry_i <= n_retry & tm_i <= n_tm) {
      # cat(glue::glue('`tm_i = {tm_i}`, '))
      tms_possible <- setdiff(idx_tm, tm_i)
      # cnd <- !all(seq_along(tms_wk1) != tms_wk1)
      tms_already_indexed <- 1:(tm_i - 1)
      tms_already_matched <- mat[tms_already_indexed, wk_i]
      tms_already_played <- mat[tm_i, 1:(wk_i - 1)]
      if(tm_i %in% tms_already_matched) {
        tmi_wki <- which(tm_i == tms_already_matched)
        if(any(tmi_wki == tms_already_played)) {
          mat[2:n_tm, wk_i] <- NA
          tm_i <- 2
          retry_i <- retry_i + 1
          cat('\n')
          # cat(glue::glue('Retry {retry_i}.'), sep = '\n')
          cat(glue::glue('Re-trying `wk_i = {wk_i}`{paste0(rep("-", 62), collapse = "")}'), sep = '\n')
        } else {
          # cat(glue::glue('`tmi_wki = {tmi_wki}`'), sep = '\n')
          mat[tm_i, wk_i] <- tmi_wki
          tm_i <- tm_i + 1
        }
      } else {
        tms_cant_match <- unique(c(tms_already_indexed, tms_already_matched, tms_already_played))
        tms_unmatched <- setdiff(tms_possible, tms_cant_match)
        # cat(glue::glue('`tms_unmatched = {paste0(tms_unmatched, collapse = ", ")}`, '), sep = '')
        n_matched <- length(tms_unmatched)
        if(n_matched == 0) {
          mat[2:n_tm, wk_i] <- NA
          tm_i <- 2
          retry_i <- retry_i + 1
          cat('\n')
          # cat(glue::glue('Retry {retry_i}.'), sep = '\n')
          cat(glue::glue('Re-trying `wk_i = {wk_i}`{paste0(rep("-", 62), collapse = "")}'), sep = '\n')
        } else {
          # Note that `sample()` will assume 1:n if n is of length 1, which we don't want.
          tmi_wki <- if(n_matched == 1) {
            tms_unmatched
          } else {
            sample(tms_unmatched, 1)
          }
          # cat(glue::glue('`tmi_wki = {tmi_wki}`'), sep = '\n')
          mat[tm_i, wk_i] <- tmi_wki
          tm_i <- tm_i + 1
        }
      }
      
    }
    wk_i <- wk_i + 1
  }
  
  # last week
  total <- Reduce(sum, 1:n_tm) - 2:n_tm
  rs <- rowSums(mat[2:n_tm, 1:(n_wk - 1)])
  tm_last <- total - rs
  mat[2:n_tm, n_wk] <- tm_last
  # list(retry_i = retry_i, mat = mat)
  mat
}
mat <- generate()
mat
rowSums(mat)