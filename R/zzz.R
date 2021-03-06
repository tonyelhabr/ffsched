
#' @seealso \url{https://r-pkgs.org/r.html?q=onLoad#when-you-do-need-side-effects}
.onLoad <- function(libname, pkgname) {
  op <- options()
  # dir_data_ext <- file.path('..', 'ffsched-data', 'data')
  op.ff <- list(
    ffsched.dir_data = file.path('inst'),
    ffsched.dir_figs = file.path('inst'),
    ffsched.league_id = 899513,
    ffsched.league_size = 10,
    ffsched.weeks_regular_season = 12,
    ffsched.season = 2020,
    ffsched.verbose = TRUE
  )
  toset <- !(names(op.ff) %in% names(op))
  if(any(toset)) options(op.ff[toset])
  
  invisible()
}
