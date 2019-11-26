
.subdir <- '2019-12'
.dir_csv <- 'output'
import_csv_for_viz <- purrr::partial(import_csv, dir = .dir_csv, subdir = .subdir, ... = )
scores <- import_csv_for_viz(file = 'scores')
scores
tms <- 
  import_csv_for_viz(file = 'tms_2019' %>% 
  mutate_at(vars(name), ~case_when(. == 'Anthony E.' ~ 'Tony E.', TRUE ~ .)) %>% 
  mutate_at(vars(name), ~forcats::fct_reorder(., tm_id))
tms
tms_lvls <- tms %>% pull(name) %>% levels()
tms_lvls
tms_abbrvs <- sprintf('%s%s', tms_lvls %>% str_sub(1, 1), tms_lvls %>% str_sub(nchar(.) - 1, -2))
tms_abbrvs
tms_names <-
  tms %>% 
  inner_join(
    tibble(name = tms_lvls, abbrv = tms_abbrvs)
  ) %>% 
  mutate_at(vars(name), ~forcats::fct_reorder(., tm_id))
tms_names

tms_scores <-
  bind_rows(
    inner_join(tms, scores %>% mutate(tm_id = tm_away_id)),
    inner_join(tms, scores %>% mutate(tm_id = tm_home_id))
  ) %>% 
  filter(!is.na(tm_winner_id)) %>% 
  arrange(tm_id, season, wk) %>% 
  mutate(
    pf = ifelse(tm_id == tm_away_id, pts_away, pts_home),
    pa = ifelse(tm_id == tm_away_id, pts_home, pts_away),
    # is_winner = ifelse(tm_id == tm_winner_id, TRUE, FALSE),
    w = ifelse(tm_id == tm_winner_id, 1L, 0L),
    l = ifelse(tm_id != tm_winner_id, 1L, 0L)
  ) %>% 
  filter(!is_playoffs) %>% 
  group_by(name) %>% 
  mutate_at(vars(pf, pa, w, l), list(cusum = cumsum)) %>% 
  ungroup() %>% 
  group_by(wk) %>% 
  arrange(desc(w_cusum), desc(pf_cusum), desc(pa_cusum)) %>% 
  mutate(rnk = row_number()) %>% 
  ungroup() %>% 
  group_by(name) %>% 
  arrange(wk, .by_group = TRUE) %>% 
  mutate(rnk_final = last(rnk)) %>% 
  ungroup()
tms_scores

col_grid <- '#333333' # grid_col, axis_col
col_fgrnd_def <- '#57c1f1' # def_fore
col_bkgrnd <- '#1e1e1e'# bkgrnd
col_fgrnd <- '#e0e0e0' # fgrnd
colors_modern_rc <- c(col_grid, col_fgrnd_def, col_bkgrnd, col_fgrnd)
# scales::show_col(colors_modern_rc, labels = FALSE)
