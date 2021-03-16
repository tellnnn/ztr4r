# is_testing <- function() identical(Sys.getenv("TESTTHAT"), "true")
# is_dev_mode <- function() exists(".__DEVTOOLS__", .getNamespace("ztr"))


# ------------------------------ #
#           set_headers          #
# ------------------------------ #
#' Set HTTP headers.
#'
#' Set HTTP headers that used for sending requests.
#'
#' Specifically, the returned HTTP header includes (1) Zotero API version and (2) Zotero API key. Please see `get_api_version()` and `get_api_key()` for details, respectively.
#'
#' @param name A `character` string. This specifies which of your API keys to be used. Even for a public library, `name` is required. `name` is passed to `get_api_key()` function.
#'
#' @return `httr::add_headers()` with "Zotero-API-Version" and "Zotero-API-Key".
#'
#' @importFrom stringr str_length
#' @importFrom httr add_headers
#'
#' @examples set_headers(name = "your_api_key_name")
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

#' Get Zotero API version
#'
#' Get Zotero API version. Currently, the version 3 is supported.
#'
#' @return Zotero API version in `integer`.
#'
#' @examples get_api_version()
get_api_version <- function() {
  # return Zotero API version
  return(3)
}


# ------------------------------ #
#          get_api_key           #
# ------------------------------ #
#' Get Zotero API key
#'
#' Get Zotero API key specified with the `name`.
#'
#'This function calls `load_api_key()` first. `load_api_key()` tries to load the cached API key specified with the `name` from a cache directory for `ztr4r` (see `api_key_path()` for its path). If `load_api_key()` successfully loads the API key, it will be used for the current session. Otherwise, `create_api_key()` will be called successively. It will create a new API key set by asking several user inputs, and store it with the `name` into the cache directory. The stored API key will be used for the current session.
#'
#' @param name A `character` string. This specifies which of your API keys to be used. Even for a public library, `name` is required.
#'
#' @return Zotero API key. Either a `character` (private library) string or `NULL` (public library).
#'
#' @importFrom stringr str_length
#'
#' @examples get_api_key(name = "your_api_key_name")
get_api_key <- function(name) {
  # validate input
  stopifnot(
    is.character(name),
    str_length(name) > 0
  )

  # load and assign an object that includes API key onto the `.env_ztr4r` environment
  .env_ztr4r$auth <- load_api_key(name = name)
  # or create an object that includes API key and assign it onto the `.env_ztr4r`
  if(is.null(.env_ztr4r$auth)) {
    .env_ztr4r$auth <- create_api_key(name = name)
  }

  # return API key
  return(.env_ztr4r$auth$api_key)
}


# ------------------------------ #
#          load_api_key          #
# ------------------------------ #
#' Load a cached Zotero API key
#'
#' Load a cached Zotero API key specified with the `name`.
#'
#' This function tries to load a cached API key under the user cached directory. The directory path will be specified with `api_key_path()`. If this function successfully loads the cached API key, it will return an R object that includes the API key and other information. Otherwise, this function always returns `NULL`.
#'
#' @param name A `character` string. This specifies which of your API keys to be used. Even for a public library, `name` is required.
#'
#' @return Either an R object that includes the API key and other information or `NULL`.
#'
#' @importFrom stringr str_length
#'
#' @examples load_api_key(name = "your_api_key_name")
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
    message("The specified API key has not been found in the user cache directory.")
    # return NULL
    return(NULL)
  }
}


# ------------------------------ #
#         create_api_key         #
# ------------------------------ #
#' Create a new API key
#'
#' This function creates a new API key with the `name` and several user inputs. Thus, this function should be called interactively.
#'
#' @param name A `character` string. This specifies the name of a new API key. Even for a public library, `name` is required to store the user/group ID, the privacy setting, and so on.
#'
#' @return An R object that includes the API key and other information
#'
#' @importFrom stringr str_length
#'
#' @examples create_api_key(name = "your_api_key_name")
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
#' Save API key
#'
#' Save an API key in the user cache directory
#'
#' This function saves the API key (`list`) created in the `create_api_key()` under the user cache directory in the `.rds` file format.
#'
#' @param name A `character` string. This specifies the name of a new API key. Even for a public library, `name` is required.
#' @param auth A `list` created in the `create_api_key()`.
#'
#' @return `NULL`
#'
#' @importFrom utils askYesNo
#'
#' @examples save_api_key(name = "your_api_key_name", auth = auth)
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
#' Get API key path
#'
#' Get the absolute path to the API key path
#'
#' @param name A `character` string. This specifies the name of a new API key. Even for a public library, `name` is required.
#'
#' @return A `character` string. This specifies the absolute path to a RDS file that stores API key.
#'
#' @importFrom stringr str_length
#' @importFrom rappdirs user_data_dir
#'
#' @examples api_key_path(name = "your_api_key_name")
api_key_path <- function(name) {
  # validate input
  stopifnot(
    is.character(name),
    str_length(name) > 0
  )

  return(
    file.path(
      user_data_dir(appname = "ztr4r", appauthor = "R"),
      paste0(name, ".rds")
    )
  )
}
