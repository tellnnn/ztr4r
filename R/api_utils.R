# is_testing <- function() identical(Sys.getenv("TESTTHAT"), "true")
# is_dev_mode <- function() exists(".__DEVTOOLS__", .getNamespace("ztr"))


# ------------------------------ #
#           set_headers          #
# ------------------------------ #
#' Set HTTP headers.
#'
#' @param name A `character` string.
#'
#' @return `httr::add_headers()`
#'
#' @importFrom stringr str_length
#' @importFrom httr add_headers
#'
#' @examples
#' \dontrun{
#' set_headers(name = "your_api_key_name")
#' }
set_headers <- function(name) {
  # validate input
  stopifnot(
    is.character(name),
    str_length(name) > 0
  )

  # return headers
  return(
    add_headers(
      "Zotero-API-Version" = get_api_version(),
      "Zotero-API-Key" = get_api_key(name = name)
    )
  )
}


# ------------------------------ #
#         get_api_version        #
# ------------------------------ #
#' Get Zotero Web API version
#'
#' @return Zotero Web API version in `integer`.
#'
#' @examples
#' \dontrun{
#' get_api_version()
#' }
get_api_version <- function() {
  return(3)
}


# ------------------------------ #
#          get_api_key           #
# ------------------------------ #
#' Get Zotero Web API key
#'
#' @param name A `character` string.
#'
#' @return Zotero Web API key in either a `character` string (private library) or `NULL` (public library).
#'
#' @importFrom stringr str_length
#'
#' @examples
#' \dontrun{
#' get_api_key(name = "your_api_key_name")
#' }
get_api_key <- function(name) {
  # validate input
  stopifnot(
    is.character(name),
    str_length(name) > 0
  )

  # load a list that contains API key and assign it onto the `.env_ztr4r`
  assign(x = "auth", value = load_api_key(name = name), envir = .env_ztr4r)
  # or create a list that contains API key and assign it onto the `.env_ztr4r`
  if(is.null(.env_ztr4r$auth)) {
    assign(x = "auth", value = create_api_key(name = name), envir = .env_ztr4r)
  }

  # return API key
  return(.env_ztr4r$auth$api_key)
}


# ------------------------------ #
#          load_api_key          #
# ------------------------------ #
#' Load a cached Zotero Web API key
#'
#' @param name A `character` string.
#'
#' @return Either a `list` that contains the API key or `NULL`.
#'
#' @importFrom stringr str_length
#'
#' @examples
#' \dontrun{
#' load_api_key(name = "your_api_key_name")
#' }
load_api_key <- function(name) {
  # validate input
  stopifnot(
    is.character(name),
    str_length(name) > 0
  )

  # get the file path
  fpath <- api_key_path(name = name)
  # check existence
  if (file.exists(fpath)) {
    # read the file and return it
    return(readRDS(file = fpath))
  } else {
    message("The specified API key has not been found in the user data directory.")
    # return NULL
    return(NULL)
  }
}


# ------------------------------ #
#         create_api_key         #
# ------------------------------ #
#' Create a new Zotero Web API key
#'
#' @param name A `character` string.
#'
#' @return A `list` that contains the API key.
#'
#' @importFrom stringr str_length
#'
#' @examples
#' \dontrun{
#' create_api_key(name = "your_api_key_name")
#' }
create_api_key <- function(name) {
  # validate input
  stopifnot(
    interactive(),
    is.character(name),
    str_length(name) > 0
  )

  # greet
  message(paste0("Creating a new API key with the name...: "), name)

  # specify the privacy setting
  privacy <- readline("Please enter privacy setting [private/public]: ")
  if (privacy == "public") {
    # specify the API key
    api_key <- NULL
    # specify the library type
    library_type <- "groups"
  } else if (privacy == "private") {
    # specify the API key
    message("please set the API key for from: https://www.zotero.org/settings/keys")
    api_key <- readline("Please enter the API key: ")
    # specify the library type
    library_type <- readline("Please enter library type [users/groups]: ")
  } else {
    stop("invalid privacy setting")
  }

  # specify the ID
  id <- readline("Please enter id: ")

  # validate privacy, api_key, library_type, and id
  stopifnot(
    privacy %in% c("private","public"), # privacy
    (is.null(api_key) || str_length(api_key) > 0), # api_key
    library_type %in% c("users", "groups"), # library_type
    str_length(id) > 0, # id
  )

  auth <- list(
    name = name,
    ID = id,
    type = library_type,
    privacy = privacy,
    key = api_key
  )

  # save auth
  save_api_key(name = name, auth = auth)

  # return auth
  return(auth)
}


# ------------------------------ #
#          save_api_key          #
# ------------------------------ #
#' Save Zotero Web API key
#'
#' @param name A `character` string.
#' @param auth A `list` created by `create_api_key()`.
#'
#' @return `NULL`
#'
#' @importFrom utils askYesNo
#'
#' @examples
#' \dontrun{
#' save_api_key(name = "your_api_key_name", auth = auth)
#' }
save_api_key <- function(name, auth) {
  # validate input
  stopifnot(
    is.character(name),
    str_length(name) > 0,
    is.list(auth),
    all(c("name", "ID", "type", "privacy", "key") %in% names(auth))
  )

  # set the file path
  fpath <- api_key_path(name = name)
  # check existence
  overwrite <- TRUE
  if (file.exists(fpath)) {
    overwrite <- askYesNo(msg = paste0("API key named as ", name, " already exists. Overwrite?"))
  }

  if (isTRUE(overwrite)) {
    saveRDS(object = auth, file = fpath)
  } else if (isFALSE(overwrite)) {
    return(NULL)
  } else {
    stop("Stop creating API key.")
  }
}


# ------------------------------ #
#          api_key_path          #
# ------------------------------ #
#' Get Zotero Web API key path
#'
#' @param name A `character` string.
#'
#' @return A `character` string.
#'
#' @importFrom stringr str_length
#'
#' @examples
#' \dontrun{
#' api_key_path(name = "your_api_key_name")
#' }
api_key_path <- function(name) {
  # validate input
  stopifnot(
    is.character(name),
    str_length(name) > 0
  )

  return(file.path(dir_path, paste0(name, ".rds")))
}
