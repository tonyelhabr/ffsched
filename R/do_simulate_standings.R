
.generate_sims_file <- function(league_id, league_size, season, weeks, sims) {
  sprintf(
    'standings_sims-league_id=%s-league_size=%s-season=%s-weeks=%s-sims=%s',
    league_id, league_size, season, weeks, sims
  )
}

#' Combine `generate_schedules` and `scrape_espn_ff_scores` results
#' 
#' Combine `generate_schedules` and `scrape_espn_ff_scores` results to come up
#' with final standing frequencies
#' 
#' @inheritParams generate_schedules
#' @inheritParams scrape_espn_ff_scores
#' @param params.schedules,params.scores List of named arguments that share 
#' names with arguments intended for this function (e.g. `ext`, `file`, `path`, etc.) 
#' that should be specific to the `generate_schedules` and `scrape_espn_ff_scores`.
#' You probably don't want to use these arguments, unless you have a speficic
#' use case (e.g re-run only 1 of the functions or name the output file
#' in a certain, custom way).
#' @export
do_simulate_standings <- 
  function(league_id = .get_league_id(),
           league_size = .get_league_size(),
           season = .get_season(),
           weeks = .get_weeks_cutoff(),
           sims = 10,
           tries = ceiling(log(sims)),
           overwrite = FALSE,
           export = TRUE,
           dir = .get_dir_data(),
           file = .generate_sims_file(league_id, league_size, season, weeks, sims),
           ext = 'parquet',
           path = .generate_path(dir, file, ext),
           f_import = arrow::read_parquet,
           f_export = arrow::write_parquet,
           params.schedules = list(),
           params.scores = list()) {
    
    path_exists <- path %>% file.exists()
    if(path_exists & !overwrite) {
      .display_info('Importing existing scores from `path = "{path}"`.')
      return(f_import(path))
    }
    
    params.schedules <-
      c(
        league_size = league_size,
        sims = sims,
        tries = tries,
        weeks = weeks,
        c(params.schedules)
      ) %>% 
      as.list()
    
    sched_sims <-
      do.call(
        generate_schedules,
        params.schedules
      )
    
    params.scores <-
      c(
        league_id = league_id,
        league_size = league_size,
        season = season,
        weeks = weeks,
        c(params.scores)
      ) %>% 
      as.list()
    
    scores <-
      do.call(
        scrape_espn_ff_scores,
        params.scores
      )
    scores
    
    scores_by_team <- 
      scores %>% 
      dplyr::select(.data$team_id, .data$team, .data$week, .data$pf) %>% 
      dplyr::filter(.data$week <= !!weeks)

    scores_sims <-
      sched_sims %>% 
      dplyr::left_join(
        scores_by_team,
        by = c('week', 'team_id')
      ) %>% 
      dplyr::left_join(
        scores_by_team %>% 
          dplyr::rename(opponent_id = .data$team_id, opponent = .data$team, pa = .data$pf),
        by = c('week', 'opponent_id')
      ) %>% 
      dplyr::mutate(
        w = dplyr::if_else(.data$pf > .data$pa, 1L, 0L)
      )

    res <-
      scores_sims %>% 
      dplyr::group_by(.data$idx_sim, .data$team, .data$team_id) %>% 
      dplyr::summarize(
        dplyr::across(c(.data$pf, .data$w), sum)
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::group_by(.data$idx_sim) %>% 
      dplyr::mutate(
        rank_w = dplyr::min_rank(-.data$w)
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::group_by(.data$idx_sim, .data$rank_w) %>% 
      dplyr::mutate(
        rank_tiebreak = dplyr::row_number(-.data$pf) - 1L
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(rank = .data$rank_w + .data$rank_tiebreak) %>% 
      dplyr::select(-.data$rank_w, -.data$rank_tiebreak)
    res
    
    if(export) {
      .display_info('Exporting simulated standings to `path = "{path}"`.')
      dir <- dirname(path)
      if(!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE)
      }
      f_export(res, path)
    }
    res
  }