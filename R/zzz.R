
#' @seealso \url{https://r-pkgs.org/r.html?q=onLoad#when-you-do-need-side-effects}
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.ff <- list(
    ffsched.dir_data = file.path('..', 'ffsched-data', 'data'),
    ffsched.dir_out = file.path('inst', 'extdata'),
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
