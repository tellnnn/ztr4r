#' ztr4r: Zotero Web API wrapper and tool set for R
#'
#' ztr4r provides users Zotero Web API wrapper in R and a tool set for manipulating the contents in a Zotero library.
#' @keywords internal
"_PACKAGE"

#' @importFrom rlang %||%
#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

# The user data directory where user information is stored.
# check if there is the user data directory
user_dir_path <- rappdirs::user_data_dir(appname = "ztr4r", appauthor = "R")
# if not, create it
if(!dir.exists(user_dir_path)) {
  dir.create(path = user_dir_path)
}
