# is_testing <- function() identical(Sys.getenv("TESTTHAT"), "true")
# is_dev_mode <- function() exists(".__DEVTOOLS__", .getNamespace("ztr"))


# ------------------------------ #
#         get_api_version        #
# ------------------------------ #
# Get Zotero Web API version
get_api_version <- function() {
  return(3)
}


# ------------------------------ #
#          load_api_key          #
# ------------------------------ #
# Load a cached Zotero Web API key
load_api_key <- function(name) {
  # validate input
  stopifnot(
    stringr::str_length(name) > 0
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
# Create a new Zotero Web API key
create_api_key <- function(name) {
  # validate input
  stopifnot(
    interactive(),
    stringr::str_length(name) > 0
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
    (is.null(api_key) || (stringr::str_length(api_key) > 0)), # api_key
    library_type %in% c("users", "groups"), # library_type
    stringr::str_length(id) > 0 # id
  )

  auth <- list(
    name = name,
    id = id,
    library_type = library_type,
    privacy = privacy,
    api_key = api_key
  )

  # save auth
  save_api_key(name = name, auth = auth)

  # return auth
  return(auth)
}


# ------------------------------ #
#          save_api_key          #
# ------------------------------ #
# Save Zotero Web API key
save_api_key <- function(name, auth) {
  # validate input
  stopifnot(
    stringr::str_length(name) > 0,
    is.list(auth),
    all(c("name", "id", "library_type", "privacy", "api_key") %in% names(auth))
  )

  # set the file path
  fpath <- api_key_path(name = name)
  # check existence
  overwrite <- TRUE
  if (file.exists(fpath)) {
    overwrite <- utils::askYesNo(msg = paste0("API key named as ", name, " already exists. Overwrite?"))
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
# Get Zotero Web API key path
api_key_path <- function(name) {
  # validate input
  stopifnot(
    stringr::str_length(name) > 0
  )

  return(file.path(dir_path, paste0(name, ".rds")))
}
