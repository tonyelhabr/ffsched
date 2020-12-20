
# library(tidyverse)
.get_verbose <- function() {
  getOption('ff.verbose')
}

.display_info <- function(x, ..., .envir = parent.frame(), .verbose = .get_verbose(), .f_glue = glue::glue_collapse) {
  if (!.verbose) {
    return(invisible(x))
  }
  x <- .f_glue(x, '\n')
  x <- glue::glue(x, .envir = .envir)
  cli::cat_line(x)
}

.display_warning <- function(x, ..., .envir = parent.frame()) {
  x <- glue::glue_collapse(x, '\n')
  x <- glue::glue(x, .envir = .envir)
  warning(x, call. = FALSE, immediate. = TRUE)
}

.display_error <- function(x, ..., .envir = parent.frame()) {
  x <- glue::glue_collapse(x, '\n')
  x <- glue::glue(x, .envir = .envir)
  cnd <- structure(class = c('usethis_error', 'error', 'condition'), list(message = x))
  stop(cnd)
}

.time_it <- function(f, ..., .name = NULL, .verbose = .get_verbose()) {

  if(is.null(.name)) {
    .name <- rev(as.character(sys.call()))[1]
  }

  function(...) {
    time_1 <- Sys.time()
    .display_info('Starting {cli::bg_black(.name)} at {cli::bg_black(time_1)}.', .verbose = .verbose)
    res <- f(...)
    time_2 <- Sys.time()
    dur <- (time_2 - time_1) %>% lubridate::as.duration()
    dur_s <- dur %>% as.numeric('seconds') %>% scales::comma(accuracy = 0.1)
    dur_m <- dur %>% as.numeric('minutes') %>% scales::comma(accuracy = 0.1)
    parenth <-
      ifelse(
        as.numeric(dur, 'seconds') >= 31L,
        glue::glue(' (~{cli::bg_black(dur_m)} minutes)') %>% as.character(),
        ''
      )
    .display_info('Finished {cli::bg_black(.name)} at {cli::bg_black(time_2)}. It took {cli::bg_black(dur_s)} seconds{parenth} to complete.', .verbose = .verbose)
    invisible(res)
  }
}
