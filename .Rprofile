
base::.First.sys()

path_r_profile <- '~/.Rprofile'
if(file.exists(path_r_profile)) {
  source(path_r_profile)
}
rm('path_r_profile')

# options(readr.num_columns = 0)
if (interactive()) {
  suppressMessages(require(devtools))
  suppressMessages(require(usethis))
}

suppressMessages(require(devtools))
suppressMessages(require(usethis))
# invisible(R.utils::sourceDirectory(file.path('R'), recursive = FALSE))
load_all()

