
scores <- teproj::import_ext_csv(file = 'scores', dir = 'output/2019-05')
scores
tms <- 
  teproj::import_ext_csv(file = 'tms_2019', dir = 'output/2019-05') %>% 
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

.x_breaks <- seq(2, 12, by = 2)
viz_bump <-
  tms_scores %>% 
  inner_join(tms_names) %>% 
  arrange(wk, desc(rnk_final)) %>% 
  ggplot() +
  aes(x = wk, y = rnk, group = name, color = name) +
  geom_line(size = 1, alpha = 0.5) +
  geom_point(size = 15, alpha = 1) +
  geom_text(
    aes(label = abbrv),
    size = 5,
    fontface = 'bold',
    color = 'black'
  ) +
  scale_y_continuous(
    trans = 'reverse',
    breaks = 1:10,
    labels = as.character(1:10)
  ) +
  scale_x_continuous(
    breaks = .x_breaks,
    labels = paste0('Week ', .x_breaks)
  ) +
  scale_color_ff() +
  guides(
    color =
      guide_legend(
        nrow = 2,
        byrow = FALSE,
        override.aes = list(size = 5)
      )
  ) +
  theme_ff() +
  theme(
    legend.position = 'top',
    legend.title = element_blank()
  ) +
  labs(
    title = 'FF Weekly League Standings, 2019',
    x = NULL,
    y = NULL
  )
viz_bump

library(gganimate)
viz_bump_anim <-
  viz_bump +
  transition_states(wk, transition_length = 1) +
  enter_fade()
viz_bump_anim 
gganimate::animate()


export_gg(
  viz_bump,
  subdir = .subdir,
  width = 10,
  height = 8
)

tms_scores_summ <-
  tms_scores %>% 
  mutate(
    pd_pos = ifelse(w == 1L, pf - pa, 0),
    pd_neg = ifelse(w == 0L, pf - pa, 0)
  ) %>% 
  group_by(name) %>% 
  summarise(
    record = sprintf('%2d - %2d', last(w_cusum), last(l_cusum)),
    rnk_final = last(rnk_final),
    pd_pos = mean(pd_pos),
    pd_neg = mean(pd_neg)
  ) %>% 
  ungroup() %>% 
  arrange(rnk_final)
tms_scores_summ

viz_tornado <-
  tms_scores_summ %>% 
  gather(key, value, matches('pd_')) %>% 
  mutate_at(vars(name), ~forcats::fct_reorder(., -rnk_final)) %>% 
  ggplot() +
  aes(x = name, y = value, fill = key) +
  geom_col(alpha = 0.6, color = NA) +
  # scale_y_continuous(trans = 'reverse') +
  scale_fill_manual(values = c('red', 'green'), labels = c('Margin in Losses', 'Margin in Wins')) +
  coord_flip() +
  theme_ff() +
  theme(
    panel.grid.major.x = element_blank(),
    legend.position = 'bottom',
    legend.title = element_blank()
  ) +
  labs(
    title = 'Average Point Differential in Wins And Losses, 2019',
    y = 'Point Differential',
    x = NULL
  )
viz_tornado

export_gg(
  viz_tornado,
  width = 10,
  height = 8
)

viz_scores_cusum_pf <-
  tms_scores %>% 
  # filter(wk >= 6) %>% 
  ggplot() +
  aes(x = wk, y = pf_cusum, group = name, color = name) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  ggrepel::geom_label_repel(
    data = tms_scores %>% filter(wk == max(wk)),
    aes(label = name),
    fill = col_bkgrnd,
    segment.color = col_fgrnd,
    nudge_x = 2,
    force = 3 
  ) +
  scale_color_ff() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(
    # breaks = seq(2, 12, by = 2),
    # limits = c(0, 15)
    breaks = seq(0, 6, by = 1),
    limits = c(1, 6)
    # breaks = seq(6, 12, by = 2),
    # limits = c(6, 14)
  ) +
  theme_ff() +
  theme(
    legend.position = 'none'
  ) +
  labs(
    title = 'FF Team Points Scored, 2019 Season',
    x = 'Week',
    y = 'Cumulative Points'
  )
viz_scores_cusum_pf

# export_gg(
#   viz_scores_cusum_pf, 
#   width = 11, 
#   height = 7
# )

lims_xy <- c(400, 750)
viz_scores_cusum_both_1 <-
  tms_scores %>% 
  filter(wk == max(wk)) %>% 
  ggplot() +
  aes(x = pf_cusum, y = pa_cusum, group = name, color = name) +
  geom_point(size = 3) +
  geom_abline(aes(intercept = 0, slope = 1), color = col_fgrnd) +
  ggforce::geom_mark_circle(
    data = tms_scores %>% filter(wk == max(wk)),
    aes(label = name),

    con.colour = col_fgrnd
  ) +
  scale_color_ff() +
  scale_y_continuous(labels = scales::comma, limits = lims_xy) +
  scale_x_continuous(labels = scales::comma, limits = lims_xy) +
  # scale_y_continuous(labels = scales::comma) +
  # scale_x_continuous(labels = scales::comma) +
  theme_ff() +
  coord_equal() +
  theme(
    legend.position = 'none'
  ) +
  labs(
    title = 'FF Team Points For and Against, 2018 Season',
    x = 'Cumulative Points For',
    y = 'Cumulative Points Against'
  )
viz_scores_cusum_both_1

# export_gg(
#   viz_scores_cusum_both_1,
#   width = 8,
#   height = 8
# )

lims_x_2 <- c(400, 725)
lims_y_2 <- c(400, 725)
lim_buffer <- 10
viz_scores_cusum_both_2 <-
  tms_scores %>% 
  filter(wk == max(wk)) %>% 
  ggplot() +
  aes(x = pf_cusum, y = pa_cusum, group = name, color = name) +
  # geom_line(size = 1) +
  geom_point(size = 3) +
  geom_vline(aes(xintercept = mean(pf_cusum)), color = col_fgrnd) +
  geom_hline(aes(yintercept = mean(pa_cusum)), color = col_fgrnd) +
  ggforce::geom_mark_circle(
    data = tms_scores %>% filter(wk == max(wk)),
    aes(label = name),
    con.colour = col_fgrnd
  ) +
  scale_color_ff() +
  scale_y_continuous(labels = scales::comma, limits = lims_y_2) +
  scale_x_continuous(labels = scales::comma, limits = lims_x_2) +
  # scale_y_continuous(labels = scales::comma) +
  # scale_x_continuous(labels = scales::comma) +
  theme_ff() +
  coord_equal() +
  geom_text(
    data =
      tibble(
        x = lims_x_2[1],
        y = lims_y_2[1],
        lab = glue::glue('Below average scoring and
                         lucky with opponent scoring')
      ),
    inherit.aes = FALSE,
    aes(x = x, y = y, label = lab),
    hjust = 0
  ) +
  geom_text(
    data =
      tibble(
        x = lims_x_2[2],
        y = lims_y_2[2],
        lab = glue::glue('Above average scoring and
                         unlucky with opponent scoring')
      ),
    inherit.aes = FALSE,
    aes(x = x, y = y, label = lab),
    hjust = 1
  ) +
  geom_text(
    data =
      tibble(
        x = lims_x_2[1],
        y = lims_y_2[2],
        lab = glue::glue('Below average scoring and
                         unlucky with opponent scoring')
      ),
    inherit.aes = FALSE,
    aes(x = x, y = y, label = lab),
    hjust = 0
  ) +
  geom_text(
    data =
      tibble(
        x = lims_x_2[2],
        y = lims_y_2[1],
        lab = glue::glue('Above average scoring and
                         lucky with opponent scoring')
      ),
    inherit.aes = FALSE,
    aes(x = x, y = y, label = lab),
    hjust = 1
  ) +
  theme(
    legend.position = 'none'
  ) +
  labs(
    title = 'FF Team Points For and Against, 2019 Season',
    x = 'Cumulative Points For',
    y = 'Cumulative Points Against'
  )
viz_scores_cusum_both_2

export_gg(
  viz_scores_cusum_both_2,
  width = 8,
  height = 8
)
