
scores <- teproj::import_ext_csv(file = 'scores', dir = 'output/2018-20')
scores
tms <- 
  teproj::import_ext_csv(file = 'tms', dir = 'output/2018-20') %>% 
  mutate_at(vars(name), ~case_when(. == 'Anthony' ~ 'Tony', TRUE ~ .)) %>% 
  mutate_at(vars(name), ~forcats::fct_reorder(., tm_id))
tms %>% pull(name) %>% levels()

scores
tms_scores <-
  bind_rows(
    inner_join(tms, scores %>% mutate(tm_id = tm_away_id)),
    inner_join(tms, scores %>% mutate(tm_id = tm_home_id))
  ) %>% 
  arrange(tm_id, season, wk) %>% 
  mutate(
    pf = dplyr::if_else(tm_id == tm_away_id, pts_away, pts_home),
    pa = dplyr::if_else(tm_id == tm_away_id, pts_home, pts_away),
    is_winner = dplyr::if_else(tm_id == tm_winner_id, TRUE, FALSE)
  )
tms_scores

tms_scores_cusum <-
  tms_scores %>%
  filter(!is_playoffs) %>% 
  group_by(name) %>% 
  mutate_at(vars(pf, pa), cumsum)
tms_scores_cusum

col_grid <- '#333333' # grid_col, axis_col
col_fgrnd_def <- '#57c1f1' # def_fore
col_bkgrnd <- '#1e1e1e'# bkgrnd
col_fgrnd <- '#e0e0e0' # fgrnd
colors_modern_rc <- c(col_grid, col_fgrnd_def, col_bkgrnd, col_fgrnd)
# scales::show_col(colors_modern_rc, labels = FALSE)

viz_scores_cusum_pf <-
  tms_scores_cusum %>% 
  # filter(wk >= 6) %>% 
  ggplot() +
  aes(x = wk, y = pf, group = name, color = name) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  ggrepel::geom_label_repel(
    data = tms_scores_cusum %>% filter(wk == max(wk)),
    aes(label = name),
    fill = col_bkgrnd,
    segment.color = col_fgrnd,
    nudge_x = 2,
    force = 3 
  ) +
  scale_color_ff() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(
    breaks = seq(2, 12, by = 2),
    limits = c(0, 15)
    # breaks = seq(6, 12, by = 2),
    # limits = c(6, 14)
  ) +
  theme_ff() +
  theme(
    legend.position = 'none'
  ) +
  labs(
    title = 'FF Team Points Scored, 2018 Season',
    x = 'Week',
    y = 'Cumulative Points'
  )
viz_scores_cusum_pf

export_gg(
  viz_scores_cusum_pf, 
  width = 11, 
  height = 7
)

lims_xy <- c(1100, 1700)
viz_scores_cusum_both_1 <-
  tms_scores_cusum %>% 
  filter(wk == max(wk)) %>% 
  ggplot() +
  aes(x = pf, y = pa, group = name, color = name) +
  geom_point(size = 3) +
  geom_abline(aes(intercept = 0, slope = 1), color = col_fgrnd) +
  ggforce::geom_mark_circle(
    data = tms_scores_cusum %>% filter(wk == max(wk)),
    aes(label = name),

    con.colour = col_fgrnd
  ) +
  scale_color_ff() +
  scale_y_continuous(labels = scales::comma, limits = lims_xy) +
  scale_x_continuous(labels = scales::comma, limits = lims_xy) +
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

export_gg(
  viz_scores_cusum_both_1,
  width = 8,
  height = 8
)

lim_buffer <- 10
viz_scores_cusum_both_2 <-
  tms_scores_cusum %>% 
  filter(wk == max(wk)) %>% 
  ggplot() +
  aes(x = pf, y = pa, group = name, color = name) +
  # geom_line(size = 1) +
  geom_point(size = 3) +
  geom_vline(aes(xintercept = mean(pf)), color = col_fgrnd) +
  geom_hline(aes(yintercept = mean(pa)), color = col_fgrnd) +
  ggforce::geom_mark_circle(
    data = tms_scores_cusum %>% filter(wk == max(wk)),
    aes(label = name),
    con.colour = col_fgrnd
  ) +
  scale_color_ff() +
  scale_y_continuous(labels = scales::comma, limits = lims_xy) +
  scale_x_continuous(labels = scales::comma, limits = lims_xy) +
  theme_ff() +
  # coord_equal() +
  geom_text(
    data = 
      tibble(
        x = lims_xy[1], 
        y = lims_xy[1], 
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
        x = lims_xy[2], 
        y = lims_xy[2], 
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
        x = lims_xy[1], 
        y = lims_xy[2], 
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
        x = lims_xy[2], 
        y = lims_xy[1], 
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
    title = 'FF Team Points For and Against, 2018 Season',
    x = 'Cumulative Points For',
    y = 'Cumulative Points Against'
  )
viz_scores_cusum_both_2

export_gg(
  viz_scores_cusum_both_2,
  width = 8,
  height = 8
)
