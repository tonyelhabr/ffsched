
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ffsched

<!-- badges: start -->
<!-- badges: end -->

`{ffsched}` can generate unique sports league schedules. If you happen
to have an ESPN fantasy football league, then `{ffsched}` can pull your
league’s scores and help you figure out the likelihood of your standings
by combining teams’ actual scores with simulated schedules. (Often the
team that scores the most points across all games in the season does not
end up winning due to “unlucky” opponent matchups!)

## Installation

You can install the released version of ffsched from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("ffsched")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("tonyelhabr/ffsched")
```

## Basic Usage

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(magrittr)
#> Warning: package 'magrittr' was built under R version 4.0.3
library(ffsched)
```

``` r
league_id <- 899513
weeks <- 12
league_size <- 10
season <- 2020
sims <- 1000
tries <- 0.1 * sims
```

Simulate 1000 unique schedules for a 10-team league for the first 12
weeks. Note that you don’t need the `league_id` for this!

``` r
set.seed(42) # For repoducibility
sched_sims <-
  generate_schedules(
    league_size = league_size,
    sims = sims,
    tries = tries
  )
```

``` r
sched_sims
#> # A tibble: 120,000 x 4
#>    idx_sim  week team_id opponent_id
#>      <int> <dbl>   <int>       <int>
#>  1       1     1       1           2
#>  2       1     1       2           1
#>  3       1     1       3           8
#>  4       1     1       4           5
#>  5       1     1       5           4
#>  6       1     1       6           7
#>  7       1     1       7           6
#>  8       1     1       8           3
#>  9       1     1       9          10
#> 10       1     1      10           9
#> # ... with 119,990 more rows
```

Get fantasy football scores from ESPN.

``` r
scores <-
  scrape_espn_ff_scores(
    league_id = league_id,
    league_size = league_size,
    season = season,
    weeks = weeks
  )
```

``` r
scores
#> # A tibble: 120 x 14
#>    team_id opponent_id team   week team_home_id team_away_id points_home
#>      <dbl>       <dbl> <chr> <dbl>        <dbl>        <dbl>       <dbl>
#>  1       1           4 The ~     1            4            1       130. 
#>  2       1           5 The ~     2            5            1       163. 
#>  3       1           3 The ~     3            3            1       119. 
#>  4       1           8 The ~     4            8            1        95.2
#>  5       1           7 The ~     5            7            1       127. 
#>  6       1           2 The ~     6            2            1       146. 
#>  7       1           6 The ~     7            6            1       101. 
#>  8       1          10 The ~     8           10            1       152. 
#>  9       1           9 The ~     9            9            1       138. 
#> 10       1           5 The ~    10            5            1       162. 
#> # ... with 110 more rows, and 7 more variables: points_away <dbl>,
#> #   team_home <chr>, team_away <chr>, team_winner_id <dbl>, pf <dbl>, pa <dbl>,
#> #   is_winner <lgl>
```

Join the simulated schedules and the actual scores together to come up
with simulated standings.

``` r
anonymize_teams <- function(data) {
  data %>% 
    mutate(
      across(team, ~sprintf('Team %02d', team_id))
    )
}
```

``` r
scores_by_team <- scores %>% select(team_id, team, week, pf)

scores_sims <-
  sched_sims %>% 
  left_join(
    scores_by_team,
    by = c('week', 'team_id')
  ) %>% 
  left_join(
    scores_by_team %>% 
      dplyr::rename(opponent_id = .data$team_id, opponent = .data$team, pa = .data$pf),
    by = c('week', 'opponent_id')
  ) %>% 
  mutate(
    w = if_else(pf > pa, 1L, 0L)
  )

standings_sims <-
  scores_sims %>% 
  group_by(idx_sim, team, team_id) %>% 
  summarize(
    across(c(pf, w), sum)
  ) %>% 
  ungroup() %>% 
  group_by(idx_sim) %>% 
  mutate(
    rank_w = min_rank(-w)
  ) %>% 
  ungroup() %>% 
  group_by(idx_sim, rank_w) %>% 
  mutate(
    rank_tiebreak = row_number(-pf) - 1L
  ) %>% 
  ungroup() %>% 
  mutate(rank = rank_w + rank_tiebreak) %>% 
  select(-rank_w, -rank_tiebreak) %>% 
  anonymize_teams()
```

``` r
standings_sims
#> # A tibble: 10,000 x 6
#>    idx_sim team    team_id    pf     w  rank
#>      <int> <chr>     <dbl> <dbl> <int> <int>
#>  1       1 Team 10      10 1336.     5     8
#>  2       1 Team 08       8 1480.     6     6
#>  3       1 Team 02       2 1561.     7     4
#>  4       1 Team 04       4 1448.     3     9
#>  5       1 Team 06       6 1609.    10     1
#>  6       1 Team 03       3 1484.     6     5
#>  7       1 Team 09       9 1562.     5     7
#>  8       1 Team 01       1 1313.     2    10
#>  9       1 Team 05       5 1620.     7     3
#> 10       1 Team 07       7 1677.     9     2
#> # ... with 9,990 more rows
```

`standings_sims` can be achieved by using the `do_simulate_standings`
function, which wraps the functionality demonstrated above.

``` r
standings_sims <-
  do_simulate_standings(
    league_id = league_id,
    league_size = league_size,
    season = season,
    weeks = weeks,
    sims = sims,
    tries = tries
  ) %>% 
  anonymize_teams()
```

``` r
standings_sims
#> # A tibble: 10,000 x 6
#>    idx_sim team    team_id    pf     w  rank
#>      <int> <chr>     <dbl> <dbl> <int> <int>
#>  1       1 Team 10      10 1336.     5     8
#>  2       1 Team 08       8 1480.     6     6
#>  3       1 Team 02       2 1561.     7     4
#>  4       1 Team 04       4 1448.     3     9
#>  5       1 Team 06       6 1609.    10     1
#>  6       1 Team 03       3 1484.     6     5
#>  7       1 Team 09       9 1562.     5     7
#>  8       1 Team 01       1 1313.     2    10
#>  9       1 Team 05       5 1620.     7     3
#> 10       1 Team 07       7 1677.     9     2
#> # ... with 9,990 more rows
```
