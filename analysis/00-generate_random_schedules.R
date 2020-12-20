
# Random first week
.initialize <- function(n_tm, n_wk = n_tm, seed = NULL, n_retry = 10, .verbose = .get_verbose()) {
  # set.seed(seed)
  mat <- matrix(nrow = n_tm, ncol = n_wk)
  tm_i <- 1
  wk_i <- 1
  retry_i <- 1
  idx_tm <- 1:n_tm
  if(!is.null(seed)) {
    set.seed(seed)
  }
  tm1_wk1 <- sample(2:n_tm, 1)
  mat[tm_i, wk_i] <- tm1_wk1

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
        # .display_info('Re-trying `wk_i = {wk_i}`{paste0(rep("-", 62), collapse = "")}')
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
  
  if(retry_i == n_retry) {
    .display_warning('Reached max number of retries for matrix initialization ({n_retry})!')
    return(NULL)
  }
  tms_possible <- setdiff(idx_tm, c(1, tm1_wk1))
  tm1_all_wks <- sample(tms_possible, size = n_wk - 1)
  
  mat[1, 2:n_wk] <- tm1_all_wks
  mat
}

# Iterate by week and team, always checking for other matched teams in the same week and previously played teams in prior weeks
generate <- function(n_tm = 10, n_wk = n_tm - 1, seed_init = NULL, seed = NULL, n_retry = 10, .verbose = .get_verbose()) {
  # set.seed(seed)
  mat <- .initialize(n_tm = n_tm, n_wk = n_wk, seed = seed_init, n_retry = n_retry, .verbose = .verbose)
  if(is.null(mat)) {
    return(mat)
  }
  if(!is.null(seed)) {
    set.seed(seed)
  }
  wk_i <- 2
  retry_i <- 1
  idx_tm <- 1:n_tm
  while(wk_i < n_wk) {
    tm_i <- 2
    while(retry_i <= n_retry & tm_i <= n_tm) {
      tms_possible <- setdiff(idx_tm, tm_i)
      tms_already_indexed <- 1:(tm_i - 1)
      tms_already_matched <- mat[tms_already_indexed, wk_i]
      tms_already_played <- mat[tm_i, 1:(wk_i - 1)]
      reset <- FALSE
      if(tm_i %in% tms_already_matched) {
        tmi_wki <- which(tm_i == tms_already_matched)
        if(any(tmi_wki == tms_already_played)) {
          reset <- TRUE
        }
      } else {
        tms_cant_match <- unique(c(tms_already_indexed, tms_already_matched, tms_already_played))
        tms_unmatched <- setdiff(tms_possible, tms_cant_match)
        n_matched <- length(tms_unmatched)
        if(n_matched == 0) {
          reset <- TRUE
        } else {
          # Note that `sample()` will assume 1:n if n is of length 1, which we don't want.
          tmi_wki <- if(n_matched == 1) {
            tms_unmatched
          } else {
            sample(tms_unmatched, 1)
          }
        }
      }
      
      if(reset) {
        mat[2:n_tm, wk_i] <- NA
        tm_i <- 2
        retry_i <- retry_i + 1
        # .display_info('Re-trying `wk_i = {wk_i}`{paste0(rep("-", 62), collapse = "")}', .verbose = .verbose)
      } else {
        mat[tm_i, wk_i] <- tmi_wki
        tm_i <- tm_i + 1
      }
    }
    wk_i <- wk_i + 1
  }
  
  if(retry_i == n_retry) {
    # browser()
    .display_warning('Reached max number of retries for matrix initialization ({n_retry})!')
    return(NULL)
  }
  
  # Last week is deterministic
  # Don't need to do the first row
  idx_not1 <- 2:n_tm
  total <- Reduce(sum, idx_tm) - idx_not1
  rs <- rowSums(mat[idx_not1, 1:(n_wk - 1)])
  tms_last <- total - rs
  mat[idx_not1, n_wk] <- tms_last
  mat
}

# TODO: Figure out why I need to check for NAs. `NULL` matrices should be returned if a valid schedule cannot be computed.
.mat_is_invalid <- function(mat) {
  is.null(mat) | any(is.na(mat))
}

do_generate <- function(n_tm = 10, n_wk_total = 12, n_sim = 10, n_try = 10, ...) {
  # n_tm = 10; n_wk_total = 12; n_sim = 10; n_try = 10
  assertthat::assert_that(n_wk_total >= n_tm)
  # n_sim = 10; n_try = 3
  n_todo <- n_sim
  try_i <- 1
  # res <- matrix(nrow = n_sim)
  res_mats <- list()

  while(try_i <= n_try & n_todo > 0) {
    .display_info('`try_i = {try_i}`')
    .display_info('`n_todo = {n_todo}`')
    # mats <- map(1:n_todo, ~generate(n_tm = n_tm, ...))
    mats <- map(1:n_todo, ~generate(n_tm = n_tm))
    mats <- mats %>% discard(.mat_is_invalid)
    n_valid <- mats %>% length()
    i <- 2
    i_dup <- c()
    while(i <= n_valid) {
      mat_i <- mats[[i]]
      j <- 1
      while(j < i) {
        dup_with_j <- all(mat_i == mats[[j]])
        if(dup_with_j) {
          # This is basically a break
          i_dup <- c(i_dup, i)
          j <- i
        } else {
          j <- j + 1
        }
      }
      # res_unique[[i]] <- mat_i
      i <- i + 1
    }
    
    if(length(i_dup) > 0) {
      for(i in i_dup) {
        mats[[idx]] <- NULL
      }
    }
    
    n_good <- mats %>% length()
    n_todo <- n_todo - n_good
    i <- 1
    # j <- sum(!is.na(res)) + 1
    j <- length(res_mats) + 1
    # .display_info('`n_good = {n_good}`')
    while(i <= n_good) {
      res_mats[[j]] <- mats[[i]]
      i <- i + 1
      j <- j + 1
    }
    try_i <- try_i + 1
  }
  
  res_wide <- 
    res_mats %>% 
    map(
      ~.x %>% 
        as_tibble() %>% 
        set_names(sprintf('wk_%02d', 1:9)) %>% 
        mutate(tm_id = row_number())
    ) %>% 
    map_dfr(bind_rows, .id = 'idx_sim') %>% 
    mutate(across(idx_sim, as.integer))
  res_wide
  
  res_tidy <-
    res_wide %>% 
    pivot_longer(
      -c(idx_sim, tm_id),
      names_to = 'wk',
      values_to = 'opp_id'
    ) %>% 
    mutate(
      across(opp_id, as.integer),
      across(wk, ~.x %>% str_remove('wk_') %>% as.integer())
    ) %>% 
    select(idx_sim, wk, tm_id, opp_id)

  n_wk_extra <- n_wk_total - n_tm + 1 # Should be 3 for us
  res_extra <-
    res_tidy %>% 
    filter(wk <= n_wk_extra) %>% 
    mutate(across(wk, ~.x + n_tm - 1L))
  res <-
    bind_rows(
      res_tidy,
      res_extra
    ) %>% 
    arrange(idx_sim, wk, tm_id)
  res
}

sched_sims <- do_generate(n_tm = 10, n_sim = 10000, n_try = 100)
sched_sims
sched_sims %>% filter(is.na(opp_id))

sched_sims %>% count(wk)
arrow::write_parquet(sched_sims, file.path('output', '2020-sched-sims.parquet'))
