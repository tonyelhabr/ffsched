
# url_scores <- 'https://fantasy.espn.com/apis/v3/games/ffl/seasons/2019/segments/0/leagues/899513?rosterForTeamId=7&view=mDraftDetail&view=mLiveScoring&view=mMatchupScore&view=mPendingTransactions&view=mPositionalRatings&view=mRoster&view=mSettings&view=mTeam&view=modular&view=mNav'

.subdir <- '2019-12'
scores_json_raw <- import_json(file = 'scores', subdir = .subdir)
scores_raw <- scores_json_raw %>% clean_json()

scores_labs <-
  scores_raw %>% 
  filter(name2 == 'matchupPeriodId') %>% 
  drop_na_cols() %>% 
  mutate(idx_intragrp = row_number(idx)) %>% 
  select(-idx) %>% 
  mutate_at(vars(value), as.integer) %>% 
  select(idx_intragrp, matchup_period_id = value)
scores_labs

sides <- c('away', 'home')
cols_sides <-
  c(
    'adjustment',
    'gamesPlayed',
    'pointByScoringPeriod',
    'teamId',
    'tiebreak',
    'totalPoints'
  )

scores_firsts <- 
  scores_raw %>% 
  filter(name2 == head(sides, 1) & name3 == head(cols_sides, 1)) %>% 
  drop_na_cols() %>% 
  mutate(idx_intragrp = row_number(idx)) %>% 
  rename(idx_first = idx) %>% 
  select(-matches('name|value'))
scores_firsts

scores_lasts <- 
  scores_raw %>% 
  filter(name2 == tail(sides, 1) & name3 == tail(cols_sides, 1)) %>% 
  drop_na_cols() %>% 
  mutate(idx_intragrp = row_number(idx)) %>% 
  rename(idx_last = idx) %>% 
  select(-matches('name|value'))
scores_lasts

scores_rngs <-
  left_join(
    scores_firsts,
    scores_lasts
  ) %>% 
  left_join(
    scores_labs
  )
scores_rngs

scores_side <-
  scores_raw %>%
  filter(name3 %in% cols_sides) %>% 
  drop_na_cols() %>%
  mutate_at(vars(value), as.numeric) %>%
  left_join(scores_rngs %>% select(idx = idx_first, idx_intragrp, matchup_period_id)) %>% 
  fill(idx_intragrp, matchup_period_id, .direction = 'down') %>% 
  select(idx_intragrp, matchup_period_id, side = name2, col = name3, value)
scores_side

scores_meta <-
  scores_raw %>%
  filter(name1 == 'schedule' & is.na(name3)) %>% 
  drop_na_cols() %>%
  left_join(
    scores_rngs %>% 
      mutate(idx = idx_last + 1L) %>% 
      select(idx, idx_intragrp, matchup_period_id)
  ) %>% 
  fill(idx_intragrp, matchup_period_id, .direction = 'down') %>% 
  select(idx_intragrp, matchup_period_id, meta = name2, value)
scores_meta

# scores_side
# scores_meta
# scores_side %>% spread(col, value) %>% left_join(scores_meta)
# scores_side %>% spread(col, value)
# id_cols = c('idx_intragrp', 'matchup_period_id'), 
scores_wide <-
  scores_side %>% 
  rename(idx = idx_intragrp) %>% 
  mutate_at(vars(col), snakecase::to_snake_case) %>% 
  tidyr::pivot_wider(
    names_from = c('side', 'col'), 
    values_from = 'value', 
    names_sep = '_'
  )
scores_wide

scores <-
  scores_wide %>%
  filter(!is.na(idx)) %>% 
  select(
    idx, 
    wk = matchup_period_id, 
    tm_away_id = away_team_id,
    tm_home_id = home_team_id,
    pts_away = away_total_points,
    pts_home = home_total_points
  ) %>% 
  unnest(cols = matches('id|pts')) %>% 
  mutate_at(vars(matches('id')), as.integer) %>% 
  # left_join(
  #   tms %>% select(tm_away_id = tm_id, tm_away_abbrev = tm_abbrev)
  # ) %>% 
  # left_join(
  #   tms %>% select(tm_home_id = tm_id, tm_home_abbrev = tm_abbrev)
  # ) %>% 
  mutate(
    # season = 2018L,
    season = 2019L,
    is_playoffs = ifelse(wk > 12, TRUE, FALSE)
  ) %>% 
  select(
    idx,
    season,
    wk,
    is_playoffs,
    matches('id$'),
    matches('abbrev$'),
    matches('^pts')
  ) %>% 
  mutate(
    tm_winner_id = case_when(
      pts_away > pts_home ~ tm_away_id,
      pts_away < pts_home ~ tm_home_id,
      TRUE ~ NA_integer_
    )
  )
scores
export_csv(scores, dir = 'data', subdir = .subdir)
