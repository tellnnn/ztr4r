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
  stopifnot(
    stringr::str_length(name) > 0
  )

  fpath <- api_key_path(name = name)

  if (file.exists(fpath)) {
    return(readRDS(file = fpath))
  } else {
    message("The specified API key has not been found in the user data directory.")
    return(NULL)
  }
}


# ------------------------------ #
#         create_api_key         #
# ------------------------------ #
# Create a new Zotero Web API key
create_api_key <- function(name) {
  stopifnot(
    interactive(),
    stringr::str_length(name) > 0
  )

  # greet
  message(paste0("Creating a new API key with the name: "), name)

  privacy <- readline("Please enter privacy setting [private/public]: ")

  if (privacy == "public") {
    api_key <- NULL
    library_type <- "groups"
  } else if (privacy == "private") {
    api_key <- readline("Please enter the API key: ")
    library_type <- readline("Please enter library type [users/groups]: ")
  } else {
    stop("invalid privacy setting")
  }

  id <- readline("Please enter id: ")

  stopifnot(
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

  save_api_key(name = name, auth = auth)

  return(auth)
}


# ------------------------------ #
#          save_api_key          #
# ------------------------------ #
# Save Zotero Web API key
save_api_key <- function(name, auth) {
  stopifnot(
    stringr::str_length(name) > 0,
    is.list(auth),
    all(c("name", "id", "library_type", "privacy", "api_key") %in% names(auth))
  )

  fpath <- api_key_path(name = name)
  saveRDS(object = auth, file = fpath)
}


# ------------------------------ #
#          api_key_path          #
# ------------------------------ #
# Get Zotero Web API key path
api_key_path <- function(name) {
  stopifnot(
    stringr::str_length(name) > 0
  )

  return(file.path(user_dir_path, paste0(name, ".rds")))
}
