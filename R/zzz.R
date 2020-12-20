
#' @seealso \url{https://r-pkgs.org/r.html?q=onLoad#when-you-do-need-side-effects}
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.ff <- list(
    ff.dir_data = 'data',
    ff.dir_out = file.path('inst', 'extdata'),
    ff.league_id = 899513,
    ff.league_size = 10,
    ff.season = 2020,
    ff.verbose = TRUE
  )
  toset <- !(names(op.ff) %in% names(op))
  if(any(toset)) options(op.ff[toset])
  
  invisible()
}

#' @seealso \url{https://github.com/hrbrmstr/hrbrthemes/blob/master/R/zzz.r}
.onAttach <- function(libname, pkgname) {
  if (.Platform$OS.type == 'windows')  { # nocov start
    if (interactive()) packageStartupMessage('Registering Windows fonts with R')
    extrafont::loadfonts('win', quiet = TRUE)
  }
}