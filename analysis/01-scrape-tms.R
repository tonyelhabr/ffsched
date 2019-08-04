
# url_tms <- 'http://fantasy.espn.com/apis/v3/games/ffl/seasons/2018/segments/0/leagues/453218??scoringPeriodId=1&view=mRoster&view=mTeam'
# resp_tms <- url_tms %>% httr::GET()
# resp_tms
# content_tms_raw <- resp_tms %>% httr::content()
# content_tms_raw
path_tms_json <- 'data-raw/tms-2018-20.json'
tms_json_raw <- path_tms_json %>% jsonlite::read_json()
# tms_json_raw
tms_raw <- tms_json_raw %>% clean_json()
do_munge_tms <- function(data, value_first = '', ...) {
  data %>% 
    select(idx, col = name2, value) %>% 
    mutate(idx_intragrp = dplyr::if_else(col == value_first, idx, NA_integer_)) %>% 
    fill(idx_intragrp, .direction = 'down') %>% 
    mutate_at(vars(idx_intragrp), ~dense_rank(.)) %>% 
    select(-idx) %>% 
    rename(idx = idx_intragrp) %>% 
    mutate_at(vars(col), snakecase::to_snake_case) %>% 
    tidyr::pivot_wider(names_from = 'col', values_from = 'value') %>% 
    select(-idx)
}
members <-
  tms_raw %>% 
  filter(name1 == 'members') %>% 
  do_munge_tms('displayName')
members
tms <-
  tms_raw %>%
  filter(name1 == 'teams' & name2 %in% c('abbrev', 'id', 'nickname', 'primaryOwner')) %>% 
  do_munge_tms('abbrev') %>% 
  mutate_at(vars(id), as.integer) %>% 
  inner_join(members %>% rename(primary_owner = id)) %>% 
  group_by(first_name) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  mutate(name = dplyr::if_else(n == 1, first_name, sprintf('%s %s.', first_name, substr(last_name, 1, 1)))) %>% 
  select(tm_id = id, name)
tms
# tms %>% datapasta::dpasta()
teproj::export_ext_csv(tms, dir = 'output')
