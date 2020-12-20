
extrafont::loadfonts(device = 'win', quiet = TRUE)
library(tidyverse)
theme_update_ff()

scores <- scrape_espn_ff_scores()

scores_by_tm <-
  scores %>%
  select(tm_id, tm, wk, pf)
# 
# .f_rename <- function(suffix) {
#   scores_by_tm %>% rename_with(~sprintf('%s_%s', .x, suffix), -c(wk)) %>% mutate(dummy = 1L)
# }
# 
# scores_perm <-
#   full_join(
#     .f_rename('1'),
#     .f_rename('2')
#   ) %>% 
#   filter(tm_1 != tm_2) %>% 
#   mutate(
#     w = if_else(pf_1 > pf_2, 1L, 0L),
#     l = if_else(pf_1 < pf_2, 1L, 0L)
#   )
# 
# scores_perm_agg <-
#   scores_perm %>% 
#   rename_with(~str_remove_all(.x, '_1'), matches('_1')) %>% 
#   rename(pa = pf_2) %>% 
#   group_by(tm_id, tm) %>% 
#   summarize(
#     n_wk = n_distinct(wk),
#     across(c(w, l, pf, pa), sum)
#   ) %>% 
#   ungroup() %>% 
#   arrange(-w)
# scores_perm_agg

# write_csv(scores, file.path('output', 'scores_2020.csv'))
sched_sims <- 
  file.path('output', '2020-sched-sims.parquet') %>% 
  arrow::read_parquet()

scores_sims <-
  sched_sims %>% 
  left_join(
    scores_by_tm
  ) %>% 
  left_join(
    scores_by_tm %>% rename(opp_id = tm_id, opp = tm, pa = pf)
  ) %>% 
  mutate(
    w = if_else(pf > pa, 1L, 0L),
    l = if_else(w == 1L, 0L, 1L)
  )
scores_sims

scores_sims_agg <-
  scores_sims %>% 
  group_by(idx_sim, tm, tm_id) %>% 
  summarize(
    across(c(pf, pa, w, l), sum)
  ) %>% 
  ungroup() %>% 
  group_by(idx_sim) %>% 
  mutate(
    rnk_w = row_number(-w), 
    rnk_pf = row_number(-pf),
    rnk = row_number(rnk_w + rnk_pf)
  ) %>% 
  ungroup() # %>% 
  # select(-rnk_w, -rnk_pf)
scores_sims_agg  

scores_sims_agg_n <-
  scores_sims_agg %>% 
  count(tm_id, tm, rnk, sort = TRUE)

scores_sims_agg_n_top <-
  scores_sims_agg_n %>% 
  group_by(rnk) %>% 
  slice_max(n, with_ties = FALSE) %>%  
  ungroup()
scores_sims_agg_n_top

scores_sims_agg_n_top <-
  scores_sims_agg_n %>% 
  group_by(tm) %>% 
  summarize(
    tot = sum(n),
    rnk_avg = sum(rnk * n)  / tot
  ) %>% 
  ungroup() %>% 
  mutate(rnk_tot = row_number(rnk_avg)) %>% 
  arrange(rnk_tot)
scores_sims_agg_n_top


scores_sims_agg_n %>% 
  left_join(
    scores_sims_agg_n_top %>% 
      select(tm, rnk_tot, rnk_avg)
  ) %>% 
  mutate(
    across(tm, ~fct_reorder(.x, rnk_tot)),
    across(rnk, ordered)
  ) %>% 
  ggplot() +
  aes(x = rnk, y = n) +
  geom_col() +
  facet_wrap(~tm, ncol = 1)


# old stuff ----
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
