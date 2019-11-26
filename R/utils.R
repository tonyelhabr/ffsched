
# NOTE: This is a simplified/custom version of `teproj::import_ext()`
.import_ext_json <-
  function(file,
           dir = getwd(),
           ext = 'json',
           path = file.path(dir, paste0(file, '.', ext)),
           ...) {
    jsonlite::read_json(path, ...)
  }

.import_json <- function(..., dir, subdir = NULL) {
  if(!is.null(subdir)) {
    dir <- file.path(dir, subdir)
  }
  .import_ext_json(..., dir = dir)
}

import_json <- purrr::partial(.import_json, dir = 'data-raw', ... = )

.dir_export <- 'output'
.import_csv <- function(..., dir, subdir = NULL) {
  if(!is.null(subdir)) {
    dir <- file.path(dir, subdir)
  }
  teproj::import_ext_csv(..., dir = dir)
}

import_csv <- purrr::partial(.import_csv, dir = .dir_export, ... = )

.export_csv <- function(..., dir = .dir_export, subdir = NULL) {
  if(!is.null(subdir)) {
    dir <- file.path(dir, subdir)
  }
  teproj::export_ext_csv(..., dir = dir)
}

export_csv <- purrr::partial(.export_csv, dir = .dir_export, ... = )

export_gg <- function(..., dir = .dir_export, subdir = NULL) {
  if(!is.null(subdir)) {
    dir <- file.path(dir, subdir)
  }
  teproj::export_ext_png(..., dir = dir)
}
