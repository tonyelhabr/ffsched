
library(ffsched)
library(tidyverse)
x <- generate_schedules(
  league_size = 14,
  weeks = 13,
  sims = 100,
  tries = 20,
  export = FALSE
)

y <- generate_schedules(
  check_dups = FALSE,
  league_size = 14,
  weeks = 13,
  sims = 100,
  tries = 50,
  export = FALSE
)

pivot_sims <- function(data) {
  data %>% 
    unite('matchup', team_id, opponent_id, sep = '@') %>% 
    pivot_wider(
      names_from = matchup,
      values_from = week
    ) 
}

count_unique_sims <- function(data) {
  data %>% 
    pivot_sims() %>% 
    select(-idx_sim) %>% 
    distinct() %>% 
    nrow()
}

identify_dups <- function(data) {
  wide <- data %>% pivot_sims()
    wide %>% 
    group_by_at(vars(-idx_sim)) %>% 
    add_tally() %>% 
    ungroup() %>% 
    select(idx_sim, n) %>% 
    filter(n > 1L)
}

x %>% distinct(idx_sim)
y %>% distinct(idx_sim)
x %>% count_unique_sims()
y %>% count_unique_sims()
x %>% identify_dups()
y %>% identify_dups()

