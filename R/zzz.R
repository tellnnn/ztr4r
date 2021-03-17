.onLoad <- function(libname, pkgname) {
  # check if there is the user data directory
  dir_path <- rappdirs::user_data_dir(appname = "ztr4r", appauthor = "R")
  # if not, create it
  if(!dir.exists(dir_path)) {
    dir.create(path = dir_path)
  }
}
