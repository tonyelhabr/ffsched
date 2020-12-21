
.get_dir_out <- function() {
  getOption('ffsched.dir_out')
}

.get_league_id <- function() {
  getOption('ffsched.league_id')
}

.get_league_size <- function() {
  getOption('ffsched.league_size')
}

.get_season <- function() {
  getOption('ffsched.season')
}

.get_weeks_cutoff <- function() {
  getOption('ffsched.weeks_regular_season')
}

.generate_path <- function(dir, file, ext) {
  file.path(dir, sprintf('%s.%s', file, ext))
}