
extrafont::loadfonts(device = 'win', quiet = TRUE)
library(tidyverse)
library(ggtext)
theme_set(ggdark::dark_theme_minimal())
theme_update(
  text = element_text(family = 'Karla', color = 'white'),
  title = element_text('Karla', size = 22, color = 'white'),
  plot.title = element_text(face = 'bold', size = 22),
  plot.subtitle = element_text(face = 'bold', size = 14, color = 'white'),
  # plot.margin = margin(10, 10, 10, 10),
  # panel.grid = element_blank(),
  axis.title = element_text(size = 14, face = 'bold', hjust = 0.99),
  axis.text = element_text(family = 'Karla', color = 'white'),
  plot.caption = ggtext::element_markdown('Karla', size = 12, hjust = 0),
  plot.caption.position = 'plot',
  panel.spacing = element_blank(),
  panel.grid.major = element_line(color = 'gray30'),
  panel.grid.minor = element_line(color = 'gray30'),
  plot.background = element_rect(fill = 'gray10', color = NA),
  plot.tag = ggtext::element_markdown(size = 12, hjust = 1),
  plot.tag.position = c(1, 0.01),
  panel.background = element_blank()
)
update_geom_defaults('text', list(family = 'Karla', size = 5, color = 'white'))

base_url <- 'http://fantasy.espn.com/apis/v3/games/ffl/seasons/2020/segments/0/leagues'
league_id <- 899513
league_size <- 10
resp_tms <- jsonlite::fromJSON(glue::glue('{base_url}/{league_id}?view=mtm'))

tms <- 
  resp_tms$teams %>% 
  jsonlite::flatten() %>% 
  as_tibble() %>% 
  select(
    tm_id = id,
    location,
    nickname,
    abbrev
  ) %>% 
  mutate(
    across(c(location, nickname, abbrev), str_trim),
    tm = sprintf('%s %s', location, nickname)
  ) %>% 
  relocate(tm_id, tm)
tms

resp_scores_init <- jsonlite::fromJSON(glue::glue('{base_url}/{league_id}?view=mMatchup'))
scores_init <-
  tibble(
    wk = resp_scores_init$schedule$matchupPeriodId,
    tm_home_id = resp_scores_init$schedule$home$teamId,
    tm_away_id = resp_scores_init$schedule$away$teamId,
    pts_home = apply(resp_scores_init$schedule$home$pointsByScoringPeriod, 1, sum, na.rm = T),
    pts_away = apply(resp_scores_init$schedule$away$pointsByScoringPeriod, 1, sum, na.rm = T)
  )
scores_init

scores_init <- 
  scores_init %>% 
  inner_join(tms %>% select(tm_home_id = tm_id, tm_home = tm)) %>% 
  inner_join(tms %>% select(tm_away_id = tm_id, tm_away = tm)) %>% 
  filter(pts_home > 0) %>% # These games haven't been played yet.
  mutate(
    season = 2020L,
    is_playoffs = ifelse(wk > 12, TRUE, FALSE),
    tm_winner_id = case_when(
      pts_away > pts_home ~ tm_away_id,
      pts_away < pts_home ~ tm_home_id,
      TRUE ~ NA_integer_
    )
  )
scores_init

scores <-
  bind_rows(
    scores_init %>% mutate(tm_id = tm_away_id, tm = tm_away),
    scores_init %>% mutate(tm_id = tm_home_id, tm = tm_home)
  ) %>% 
  arrange(tm_id, season, wk) %>% 
  mutate(
    tm_opp_id = if_else(tm_id == tm_home_id, tm_away_id, tm_home_id),
    pf = dplyr::if_else(tm_id == tm_away_id, pts_away, pts_home),
    pa = dplyr::if_else(tm_id == tm_away_id, pts_home, pts_away),
    is_winner = dplyr::if_else(tm_id == tm_winner_id, TRUE, FALSE)
  ) %>% 
  relocate(tm_id, tm_opp_id, tm) 
scores

scores_by_tm <- 
  scores %>% 
  select(tm_id, tm, wk, pf)

.f_rename <- function(suffix) {
  scores_by_tm %>% rename_with(~sprintf('%s_%s', .x, suffix), -c(wk)) %>% mutate(dummy = 1L)
}

scores_perm <-
  full_join(
    .f_rename('1'),
    .f_rename('2')
  ) %>% 
  filter(tm_1 != tm_2) %>% 
  mutate(
    w = if_else(pf_1 > pf_2, 1L, 0L),
    l = if_else(pf_1 < pf_2, 1L, 0L)
  )

scores_perm_agg <-
  scores_perm %>% 
  rename_with(~str_remove_all(.x, '_1'), matches('_1')) %>% 
  rename(pa = pf_2) %>% 
  group_by(tm_id, tm) %>% 
  summarize(
    n_wk = n_distinct(wk),
    across(c(w, l, pf, pa), sum)
  ) %>% 
  ungroup() %>% 
  arrange(-w)
scores_perm_agg


scores_cusum <-
  scores %>%
  filter(!is_playoffs) %>% 
  group_by(tm) %>% 
  mutate(
    across(c(pf, pa), cumsum),
    w = cumsum(is_winner),
    l = wk - w
  ) %>% 
  ungroup()
scores_cusum

col_grid <- '#333333' # grid_col, axis_col
col_fgrnd_def <- '#57c1f1' # def_fore
col_bkgrnd <- '#1e1e1e'# bkgrnd
col_fgrnd <- '#e0e0e0' # fgrnd
colors_modern_rc <- c(col_grid, col_fgrnd_def, col_bkgrnd, col_fgrnd)
# scales::show_col(colors_modern_rc, labels = FALSE)

lims_xy <- c(600, 1000)
lim_buffer <- 10

scores_cusum_viz <-
  scores_cusum %>% 
  filter(wk == max(wk)) %>% 
  mutate(
    across(tm, ~case_when(tm == 'Tony El Tigre' ~ .x, TRUE ~ sprintf('Team %d', tm_id))),
    across(tm, ~sprintf('%s (%d-%d)', .x, w, l))
  )

viz <-
  scores_cusum_viz %>% 
  ggplot() +
  aes(x = pf, y = pa, group = tm) +
  # geom_line(size = 1) +
  geom_point(
    data = scores_cusum_viz %>% filter(tm_id != 7L),
    size = 3, color = 'white'
  ) +
  geom_point(
    data = scores_cusum_viz %>% filter(tm_id == 7L),
    size = 3, color = '#ffff7f'
  ) +
  geom_vline(aes(xintercept = mean(pf)), color = col_fgrnd) +
  geom_hline(aes(yintercept = mean(pa)), color = col_fgrnd) +
  ggforce::geom_mark_circle(
    data = scores_cusum_viz %>% filter(tm_id != 7L),
    aes(label = tm),
    color = 'white',
    label.family = 'Karla',
    con.colour = col_fgrnd
  ) +
  ggforce::geom_mark_circle(
    data = scores_cusum_viz %>% filter(tm_id == 7L),
    aes(label = tm),
    color = 'yellow',
    label.family = 'Karla',
    label.fill = '#ffff7f',
    con.colour = '#ffff7f'
  ) +
  # scale_color_ff() +
  scale_y_continuous(labels = scales::comma, limits = lims_xy) +
  scale_x_continuous(labels = scales::comma, limits = lims_xy) +
  # theme_ff() +
  # coord_equal() +
  geom_text(
    data = 
      tibble(
        x = lims_xy[1], 
        y = lims_xy[1], 
        lab = glue::glue('Below average scoring and 
                         lucky with opponent scoring')
      ),
    color = col_fgrnd_def,
    inherit.aes = FALSE,
    aes(x = x, y = y, label = lab),
    hjust = 0
  ) +
  geom_text(
    data = 
      tibble(
        x = lims_xy[2], 
        y = lims_xy[2], 
        lab = glue::glue('Above average scoring and 
                         unlucky with opponent scoring')
      ),
    inherit.aes = FALSE,
    color = col_fgrnd_def,
    aes(x = x, y = y, label = lab),
    hjust = 1
  ) +
  geom_text(
    data = 
      tibble(
        x = lims_xy[1], 
        y = lims_xy[2], 
        lab = glue::glue('Below average scoring and 
                         unlucky with opponent scoring')
      ),
    inherit.aes = FALSE,
    color = col_fgrnd_def,
    aes(x = x, y = y, label = lab),
    hjust = 0
  ) +
  geom_text(
    data = 
      tibble(
        x = lims_xy[2], 
        y = lims_xy[1], 
        lab = glue::glue('Above average scoring and 
                         lucky with opponent scoring')
      ),
    inherit.aes = FALSE,
    color = col_fgrnd_def,
    aes(x = x, y = y, label = lab),
    hjust = 1
  ) +
  guides(color = FALSE) +
  theme(
    legend.position = 'none'
  ) +
  labs(
    title = 'Fantasy Football Points For vs. Points Against',
    caption = '**Viz:** @TonyElHabr',
    x = 'Points For',
    y = 'Points Against'
  )
viz

.dir_plot <- here::here()
path <- fs::path(.dir_plot, 'ff_2020_06.png')
ggsave(plot = viz, filename = path, width = 11, height = 11, type = 'cairo')
