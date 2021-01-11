
.generate_espn_ff_scores_file <- function(league_id, league_size, season, weeks) {
  sprintf('scores-league_id=%s-league_size=%s-season=%s-weeks=%02d', league_id, league_size, season, weeks)
}

.coerce_local_path <- function(local, path, url, suffix = c('teams', 'scores')) {
  # suffix <- match.arg(suffix)
  if(local) {
    path_exists <- path %>% file.exists()
    if(!path_exists) {
      .display_error('`path_{suffix} = "{path}"` must exist and be a JSON file downloaded from {url}')
    }
  } else {
    path <- url
  }
  path
}

#' Scrape ESPN fantasy football scores
#' 
#' @param league_id Number for ESPN league. Probably 6 digits. Can be set globally
#' in the options. See `ffsched.league_id`.
#' @param season  Season for which to scrape. Can be set globally in the options. 
#' See `ffsched.season`.
#' @param local,path_teams,path_scores If your league is not public, you will have 
#' to download the appropriate JSON files, set `local = TRUE`,
#' and point `path_teams` and `path_scores` to these JSON files.
#' The JSON for `path_teams` should downloaded from
#' \url{http://fantasy.espn.com/apis/v3/games/ffl/seasons/{season}/segments/0/leagues/{league_id}?view=mtm}
#' and the JSON for `path_scores` should be downloaded from
#' \url{http://fantasy.espn.com/apis/v3/games/ffl/seasons/{season}/segments/0/leagues/{league_id}?view=mMatchup}.
#' @inheritParams generate_schedules
#' @export
#' @seealso \url{https://gist.github.com/lbenz730/ea7d5bce0a36fe66c4241c8facd6c153}
scrape_espn_ff_scores <-
  function(league_id = .get_league_id(),
           league_size = .get_league_size(),
           season = .get_season(),
           weeks = .get_weeks_cutoff(),
           local = FALSE,
           path_teams = NULL,
           path_scores = NULL,
           ...,
           overwrite = FALSE,
           export = TRUE,
           dir = .get_dir_data(),
           file = .generate_espn_ff_scores_file(league_id, league_size, season, weeks),
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
    url_teams <- glue::glue('{base_url}/{league_id}?view=mtm')
    url_scores <- glue::glue('{base_url}/{league_id}?view=mMatchup')
    path_teams <- .coerce_local_path(local = local, path = path_teams, url = url_teams, suffix = 'teams')
    path_scores <- .coerce_local_path(local = local, path = path_scores, url = url_scores, suffix = 'scores')
    
    resp_teams <-
      path_teams %>% 
      jsonlite::fromJSON()
    
    resp_scores_init <- 
      path_scores %>% 
      jsonlite::fromJSON()
    
    teams <-
      resp_teams$teams %>%
      jsonlite::flatten() %>% 
      tibble::as_tibble() %>% 
      dplyr::select(
        team_id = .data$id,
        .data$location,
        .data$nickname,
        .data$abbrev
      ) %>% 
      dplyr::mutate(
        dplyr::across(c(.data$location, .data$nickname, .data$abbrev), stringr::str_trim),
        team = sprintf('%s %s', .data$location, .data$nickname)
      ) %>% 
      dplyr::relocate(.data$team_id, .data$team)

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
          dplyr::select(team_home_id = .data$team_id, team_home = .data$team),
        by = 'team_home_id'
      ) %>% 
      dplyr::inner_join(
        teams %>% 
          dplyr::select(team_away_id = .data$team_id, team_away = .data$team),
        by = 'team_away_id'
      )
    
    if(!is.null(weeks)) {
      scores_init <-
        scores_init %>% 
        # These games haven't been played yet.
        # dplyr::filter(.data$points_home > 0) %>% 
        dplyr::filter(.data$week <= !!weeks)
    }
    
    scores_init <-
      scores_init %>% 
      dplyr::mutate(
        team_winner_id = dplyr::case_when(
          .data$points_away > .data$points_home ~ .data$team_away_id,
          .data$points_away < .data$points_home ~ .data$team_home_id,
          TRUE ~ NA_integer_
        )
      )
    
    scores <-
      dplyr::bind_rows(
        scores_init %>% dplyr::mutate(team_id = .data$team_away_id, team = .data$team_away),
        scores_init %>% dplyr::mutate(team_id = .data$team_home_id, team = .data$team_home)
      ) %>% 
      dplyr::arrange(.data$team_id, .data$week) %>% 
      dplyr::mutate(
        opponent_id = dplyr::if_else(.data$team_id == .data$team_home_id, .data$team_away_id, .data$team_home_id),
        pf = dplyr::if_else(.data$team_id == .data$team_away_id, .data$points_away, .data$points_home),
        pa = dplyr::if_else(.data$team_id == .data$team_away_id, .data$points_home, .data$points_away),
        is_winner = dplyr::if_else(.data$team_id == .data$team_winner_id, TRUE, FALSE)
      ) %>% 
      dplyr::relocate(.data$team_id, .data$opponent_id, .data$team)
    
    if(export) {
      .display_info('Exporting scores to `path = "{path}"`.')
      dir <- dirname(path)
      if(!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE)
      }
      f_export(scores, path)
    }
    scores
  }