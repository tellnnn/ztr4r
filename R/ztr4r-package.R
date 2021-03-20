#' ztr4r: Zotero Web API wrapper and tool set for R
#'
#' ztr4r provides users Zotero Web API wrapper in R and a tool set for manipulating the contents in a Zotero library.
#'
#' @examples
#' \dontrun{
#' library(ztr4r)
#' }
#' @keywords internal
"_PACKAGE"

#' @import rlang
NULL

# The internal environment where user information is stored for a session.
.env_ztr4r <- new.env(parent = emptyenv())

# The user data directory where user information is stored.
# check if there is the user data directory
dir_path <- rappdirs::user_data_dir(appname = "ztr4r", appauthor = "R")
# if not, create it
if(!dir.exists(dir_path)) {
  dir.create(path = dir_path)
}
