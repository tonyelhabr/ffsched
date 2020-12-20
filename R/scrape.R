
#' Scrape ESPN fantasy football scores
#' 
#' @param league_id Number for ESPN league. Probably 6 digits. Can be set globally
#' in the options. See `ff.league_id`.
#' @param season  Season for which to scrape. Can be set globally in the options. 
#' See `ff.season`.
#' @inheritParams generate_schedules
#' @export
#' @seealso \url{https://gist.github.com/lbenz730/ea7d5bce0a36fe66c4241c8facd6c153}
scrape_espn_ff_scores <-
  function(league_id = .get_league_id(),
           league_size = .get_league_size(),
           season = .get_season(),
           ...,
           overwrite = FALSE,
           export = TRUE,
           dir = .get_dir_out(),
           file = sprintf('scores-league_id=%s-league_size=%s-season=%s', league_id, league_size, season),
           ext = 'csv',
           path = file.path(dir, sprintf('%s.%s', file, ext)),
           f_import = readr::read_csv,
           f_export = readr::write_csv) {
    
    path_exists <- path %>% file.exists()
    if(path_exists & !overwrite) {
      .display_info('Importing existing scores from `path = "{path}"`.')
      return(f_import(path))
    }
    
    base_url <-
      glue::glue('http://fantasy.espn.com/apis/v3/games/ffl/seasons/{season}/segments/0/leagues')
    resp_teams <-
      glue::glue('{base_url}/{league_id}?view=mtm') %>% 
      jsonlite::fromJSON()
    
    teams <-
      resp_teams$teams %>%
      jsonlite::flatten() %>% 
      tibble::as_tibble() %>% 
      dplyr::select(
        team_id = id,
        location,
        nickname,
        abbrev
      ) %>% 
      dplyr::mutate(
        dplyr::across(c(location, nickname, abbrev), stringr::str_trim),
        team = sprintf('%s %s', location, nickname)
      ) %>% 
      dplyr::relocate(team_id, team)
    
    resp_scores_init <- 
      glue::glue('{base_url}/{league_id}?view=mMatchup') %>% 
      jsonlite::fromJSON()
    scores_init <-
      tibble::tibble(
        week = resp_scores_init$schedule$matchupPeriodId,
        team_home_id = resp_scores_init$schedule$home$teamId,
        team_away_id = resp_scores_init$schedule$away$teamId,
        points_home = apply(resp_scores_init$schedule$home$pointsByScoringPeriod, 1, sum, na.rm = T),
        points_away = apply(resp_scores_init$schedule$away$pointsByScoringPeriod, 1, sum, na.rm = T)
      )
    
    scores_init <- 
      scores_init %>% 
      dplyr::inner_join(
        teams %>% 
          dplyr::select(team_home_id = team_id, team_home = team),
        by = 'team_homey_id'
      ) %>% 
      dplyr::inner_join(
        teams %>% 
          dplyr::select(team_away_id = team_id, team_away = team),
        by = 'team_away_id'
      ) %>% 
      # These games haven't been played yet.
      dplyr::filter(points_home > 0) %>% 
      dplyr::mutate(
        team_winner_id = dplyr::case_when(
          points_away > points_home ~ team_away_id,
          points_away < points_home ~ team_home_id,
          TRUE ~ NA_integer_
        )
      )
    
    scores <-
      dplyr::bind_rows(
        scores_init %>% dplyr::mutate(team_id = team_away_id, team = team_away),
        scores_init %>% dplyr::mutate(team_id = team_home_id, team = team_home)
      ) %>% 
      dplyr::arrange(team_id, season, week) %>% 
      dplyr::mutate(
        opponent_id = dplyr::if_else(team_id == team_home_id, team_away_id, team_home_id),
        pf = dplyr::if_else(team_id == team_away_id, points_away, points_home),
        pa = dplyr::if_else(team_id == team_away_id, points_home, points_away),
        is_winner = dplyr::if_else(team_id == team_winner_id, TRUE, FALSE)
      ) %>% 
      dplyr::relocate(team_id, opponent_id, team)
    
    if(overwrite & export) {
      .display_info('Exporting scores to `path = "{path}"`.')
      dir <- dirname(path)
      if(!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE)
      }
      f_export(scores, path)
    }
    scores
  }