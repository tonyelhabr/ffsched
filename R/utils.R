
# NOTE: This is a simplified/custom version of `teproj::import_ext()`
.import_ext_json <-
  function(file,
           dir = getwd(),
           ext = 'json',
           path = file.path(dir, paste0(file, '.', ext)),
           ...) {
    jsonlite::read_json(path, ...)
  }

.dir_import <- 'data-raw'
import_json <- function(..., dir = .dir_import, subdir = NULL) {
  if(!is.null(subdir)) {
    dir <- file.path(dir, subdir)
  }
  .import_ext_json(..., dir = dir)
}

import_csv <- function(..., dir = .dir_import, subdir = NULL) {
  if(!is.null(subdir)) {
    dir <- file.path(dir, subdir)
  }
  teproj::import_ext_csv(..., dir = dir)
}


.dir_export <- 'output'
export_csv <- function(..., dir = .dir_export, subdir = NULL) {
  if(!is.null(subdir)) {
    dir <- file.path(dir, subdir)
  }
  teproj::export_ext_csv(..., dir = dir)
}


export_gg <- function(..., dir = .dir_export, subdir = NULL) {
  if(!is.null(subdir)) {
    dir <- file.path(dir, subdir)
  }
  teproj::export_ext_png(...,dir = dir)
}
