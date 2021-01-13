
# Random first week
.initialize <- function(league_size = .get_league_size(), seed = NULL, retries = 10) {
  
  weeks <- league_size - 1
  mat <- matrix(nrow = league_size, ncol = weeks)
  team_i <- 1
  week_i <- 1
  retry_i <- 1
  idx_team <- 1:league_size
  if(!is.null(seed)) {
    set.seed(seed)
  }
  team1_week1 <- sample(2:league_size, 1, replace = FALSE)
  mat[team_i, week_i] <- team1_week1
  
  team_i <- 2
  while(retry_i <= retries & team_i <= league_size) {
    teams_possible <- setdiff(idx_team, team_i)
    teams_already_indexed <- 1:(team_i - 1)
    teams_already_matched <- mat[teams_already_indexed, week_i]
    if(team_i %in% teams_already_matched) {
      teami_weeki <- which(team_i == teams_already_matched)
      mat[team_i, week_i] <- teami_weeki
      team_i <- team_i + 1
    } else {
      teams_cant_match <- unique(c(teams_already_indexed, teams_already_matched))
      teams_unmatched <- setdiff(teams_possible, teams_cant_match)
      n_matched <- length(teams_unmatched)
      if(n_matched == 0) {
        mat[2:league_size, week_i] <- NA
        team_i <- 2
        # .display_info('Re-trying `week_i = {week_i}`{paste0(rep("-", 62), collapse = "")}')
      } else {
        teami_weeki <- if(n_matched == 1) {
          teams_unmatched
        } else {
          sample(teams_unmatched, 1)
        }
        # cat(glue::glue('`teami_weeki = {teami_weeki}`'), sep = '\n')
        mat[team_i, week_i] <- teami_weeki
        team_i <- team_i + 1
      }
    }
  }
  
  if(retry_i == retries) {
    # .display_warning('Reached max number of retries for matrix initialization ({retries})!')
    browser()
    return(NULL)
  }
  teams_possible <- setdiff(idx_team, c(1, team1_week1))
  team1_all_weeks <- sample(teams_possible, size = length(teams_possible))
  
  mat[1, 2:weeks] <- team1_all_weeks
  
  mat
}

#' Generate a single schedule
#' 
#' @details Iterate by week and team, always checking for other matched teams in 
#' the same week and previously played teams in prior weeks
#' @inheritParams generate_schedules
#' @param seed Seed to use
#' @param seed_init Initial seed to use, passed to the non-exported `.initialize()` function.
#' @param retries Number of retries for a single schedule
#' @param retries_init Initial number of retries, passed to the non-exported `.initialize()` function.
#' @export
generate_schedule <-
  function(league_size = .get_league_size(),
           # weeks = league_size,
           seed = NULL,
           seed_init = NULL,
           retries = league_size^2,
           retries_init = retries) {
    
    mat <-
      .initialize(
        league_size = league_size,
        # weeks = league_size - 1,
        seed = seed_init,
        retries = retries_init
      )
    if (is.null(mat)) {
      return(mat)
    }
    if(!is.null(seed)) {
      set.seed(seed)
    }
    week_i <- 2
    retry_i <- 1
    idx_team <- 1:league_size
    weeks <- league_size - 1
    while(week_i < weeks) {
      team_i <- 2
      while(retry_i <= retries & team_i <= league_size) {
        teams_possible <- setdiff(idx_team, team_i)
        teams_already_indexed <- 1:(team_i - 1)
        teams_already_matched <- mat[teams_already_indexed, week_i]
        teams_already_played <- mat[team_i, 1:(week_i - 1)]
        reset <- FALSE
        if(team_i %in% teams_already_matched) {
          teami_weeki <- which(team_i == teams_already_matched)
          if(any(teami_weeki == teams_already_played)) {
            reset <- TRUE
          }
        } else {
          teams_cant_match <-
            unique(c(teams_already_indexed, teams_already_matched, teams_already_played))
          teams_unmatched <- setdiff(teams_possible, teams_cant_match)
          n_matched <- length(teams_unmatched)
          if (n_matched == 0) {
            reset <- TRUE
          } else {
            # Note that `sample()` will assume 1:n if n is of length 1, which we don't want.
            teami_weeki <- if(n_matched == 1) {
              teams_unmatched
            } else {
              sample(teams_unmatched, 1)
            }
          }
        }
        
        if(reset) {
          mat[2:league_size, week_i] <- NA
          team_i <- 2
          retry_i <- retry_i + 1
          # .display_info('Re-trying `week_i = {week_i}`{paste0(rep("-", 62), collapse = "")}', .verbose = .verbose)
        } else {
          mat[team_i, week_i] <- teami_weeki
          team_i <- team_i + 1
        }
      }
      week_i <- week_i + 1
    }
    
    if(retry_i > retries) {
      # browser()
      # .display_warning('Reached max number of retries for `generate_schedule()` ({retries})!')
      return(NULL)
    }
    
    # Last week is deterministic. Also, don't need to do the first row
    idx_not1 <- 2:league_size
    total <- Reduce(sum, idx_team) - idx_not1
    rs <- rowSums(mat[idx_not1, 1:(weeks - 1)])
    teams_last <- total - rs
    mat[idx_not1, weeks] <- teams_last
    mat
  }

# TODO: Figure out why I need to check for NAs. `NULL` matrices should be returned if a valid schedule cannot be computed.
.mat_is_invalid <- function(mat) {
  cnd2 <- any(is.na(mat))
  if(cnd2) {
    browser()
  }
  is.null(mat) | any(is.na(mat))
}

.generate_schedules_file <- function(league_size, weeks, sims) {
  sprintf('schedules-league_size=%s-weeks=%s-sims=%s', league_size, weeks, sims)
}

#' Generate schedules
#' 
#' Generate unique schedules for teams, presumably for a fantasy football league,
#' but it doesn't actually have to be for that. Points are not simulated, just
#' schedules
#' 
#' @details This function basically uses a brute force approach, hence the `tries`
#' parameter.
#' @param league_size Number of teams in the league. Can be set globally
#' in the options. See `ffsched.league_size`.
#' @param weeks How many weeks are in schedule. Presently, this function requires
#' that `league_size <= league_size < (2 * (league_size - 1))`.
#' @param sims How many unique simulations to generate.
#' @param tries How many times to re-try.
#' @param check_dups Whether to check for duplicates. It's recommended to leave this
#' as `TRUE` to ensure that you get unique results, but that may not be what you
#' desire.
#' @param ... Additional parameters passed to `generate_schedule()`
#' @param overwrite Whether to overwrite existing file at `path`, if it exists.
#' @param export Whether to export.
#' @param dir Directory to use to generate `path` if `path` is not explicitly provided.
#' @param file Filename (without extension) to generate `path` if `path` is not explicitly provided.
#' @param ext File extension to use to generate `path` if `path` is not explicitly provided.
#' @param path Path to export to.
#' @param f_import Function to import with if file exists and `overwrite = TRUE`.
#' @param f_export Function to export with if `export = TRUE` .
#' @export
generate_schedules <-
  function(league_size = .get_league_size(),
           weeks = .get_weeks_cutoff(),
           sims = 10,
           tries = ceiling(log(sims)),
           check_dups = TRUE,
           ...,
           overwrite = FALSE,
           export = TRUE,
           dir = .get_dir_data(),
           file = .generate_schedules_file(league_size, weeks, sims),
           ext = 'parquet',
           path = file.path(dir, sprintf('%s.%s', file, ext)),
           f_import = arrow::read_parquet,
           f_export = arrow::write_parquet) {
    
    stopifnot(weeks >= league_size)
    stopifnot(weeks <= (2 * (league_size - 1)))
    
    path_exists <- path %>% file.exists()
    if(path_exists & !overwrite) {
      .display_info('Importing simulated schedules from `path = "{path}"`.')
      return(f_import(path))
    }
    
    res_mats <- list()
    loop_i <- 1
    try_i <- 1
    n_todo <- sims
    
    while(try_i <= tries & n_todo > 0) {
      .display_info('`try_i = {try_i}` ({tries - try_i} tries remaining)')
      
      mats <- 
        purrr::map(
          1:n_todo, 
          ~generate_schedule(league_size = league_size, ...)
        )
      mats <- mats %>% purrr::discard(.mat_is_invalid)
      
      n_valid <- mats %>% length()
      i <- 2
      
      if(check_dups) {
        i_dup <- c()
        while(i <= n_valid) {
          mat_i <- mats[[i]]
          j <- 1
          while(j < i) {
            # browser()
            dup_with_j <- all(mat_i == mats[[j]])
            if(dup_with_j) {
              # This is basically a break
              i_dup <- c(i_dup, i)
              j <- i
            } else {
              j <- j + 1
            }
          }
          i <- i + 1
        }
        
        # Check for duplicates.
        if(length(i_dup) > 0) {
          for(i in i_dup) {
            mats[[i]] <- NULL
          }
        }
      }
      
      n_good <- mats %>% length()
      n_todo <- n_todo - n_good
      i <- 1
      # `j` starts after the last inserted matrix into `res_mats`.
      j <- length(res_mats) + 1
      while(i <= n_good) {
        res_mats[[j]] <- mats[[i]]
        i <- i + 1
        j <- j + 1
      }
      
      if(n_todo > 0) {
        .display_info('`n_todo = {n_todo}` in `loop_i = {loop_i}`.')
        try_i <- try_i + 1
      }
      }
    
    if(try_i >= tries) {
      .display_info('Stopped early due to reaching number of tries ({tries})')
    }

    .display_info('Converting individual schedulings into one big, tidy data frame. This may take a while!')
    
    res_wide <- 
      res_mats %>% 
      purrr::map(
        ~.x %>% 
          tibble::as_tibble() %>% 
          purrr::set_names(sprintf('week_%02d', 1:9)) %>% 
          dplyr::mutate(team_id = dplyr::row_number())
      ) %>% 
      purrr::map_dfr(dplyr::bind_rows, .id = 'idx_sim') %>% 
      dplyr::mutate(dplyr::across(.data$idx_sim, as.integer))
    res_wide
    
    res_tidy <-
      res_wide %>% 
      tidyr::pivot_longer(
        -c(.data$idx_sim, .data$team_id),
        names_to = 'week',
        values_to = 'opponent_id'
      ) %>% 
      dplyr::mutate(
        dplyr::across(.data$opponent_id, as.integer),
        dplyr::across(.data$week, ~.x %>% stringr::str_remove('week_') %>% as.integer())
      ) %>% 
      dplyr::select(.data$idx_sim, .data$week, .data$team_id, .data$opponent_id)
    
    weeks_extra <- weeks - league_size + 1
    if(weeks_extra <= 0) {
      return(res_tidy)
    }
    
    .display_info('Adding {weeks_extra} weeks by coping first {weeks_extra} weeks.')
    res_extra <-
      res_tidy %>% 
      dplyr::filter(.data$week <= !!weeks_extra) %>% 
      dplyr::mutate(dplyr::across(.data$week, ~.x + !!league_size - 1L))
    
    res <-
      dplyr::bind_rows(
        res_tidy,
        res_extra
      ) %>% 
      dplyr::arrange(.data$idx_sim, .data$week, .data$team_id)
    
    if(export) {
      .display_info('Exporting simulated schedules to `path = "{path}"`.')
      dir <- dirname(path)
      if(!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE)
      }
      f_export(res, path)
    }
    res
  }

