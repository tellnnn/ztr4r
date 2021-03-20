# ------------------------------ #
#      ztr_read_collections      #
# ------------------------------ #
#' Read collections
#'
#' @param zotero_object `Zotero` object
#' @param top Specify collection levels (default: `FALSE`)
#' * `FALSE`: Collections in the library
#' * `TRUE `: \strong{Top-level} collections in the library
#' @param ... Query inputs. See \strong{Read Requests} and \strong{Sorting and Pagination} in the [Zotero Web API Documents](https://www.zotero.org/support/dev/web_api/v3/basics) for available query.
#' @param timeout Seconds for the timeout of request (default: `20`).
#'
#' @return `httr::response` object
#' @export
#'
#' @examples
#' \dontrun{
#' ztr <- Zotero$new("ztr4r")
#'
#' # simple example
#' ztr %>%
#'   ztr_read_collections()
#'
#' # example with query
#' ztr %>%
#'   ztr_read_collections(
#'     top = TRUE,
#'     format = "json", sort = "dateAdded"
#'   )
#' }
ztr_read_collections <- function(zotero_object, top = FALSE, ..., timeout = 20) {
  zotero_object$request(
    verb = "GET",
    path = c("collections", if (top) {"top"}),
    ...,
    timeout = 20
  )
}


# ------------------------------ #
#      ztr_read_collection       #
# ------------------------------ #
#' Read a specific collection or sub-collections within a specific collection
#'
#' @param zotero_object `Zotero` object
#' @param collection Collection key
#' @param sub Specify whether to retrieve sub-collections within a specific collection. Otherwise, a specific collection will be retrieved.
#' @param ... Query inputs. See \strong{Read Requests} and \strong{Sorting and Pagination} in the [Zotero Web API Documents](https://www.zotero.org/support/dev/web_api/v3/basics) for available query.
#' @param timeout Seconds for the timeout of request (default: `20`).
#'
#' @return `httr::response` object
#' @export
#'
#' @examples
#' \dontrun{
#' ztr <- Zotero$new("ztr4r")
#'
#' # simple example
#' ztr %>%
#'   ztr_read_collection(collection = "foo")
#'
#' # example with query
#' ztr %>%
#'   ztr_read_collection(
#'     collection = "foo", sub = TRUE,
#'     format = "json", sort = "dateAdded"
#'   )
#' }
ztr_read_collection <- function(zotero_object, collection, sub = FALSE, ..., timeout = 20) {
  stopifnot(
    is.character(collection),
    stringr::str_length(collection) > 0
  )

  zotero_object$request(
    verb = "GET",
    path = c("collections", collection, if (sub) {"collections"}),
    ...,
    timeout = 20
  )
}


# ------------------------------ #
#         ztr_read_items         #
# ------------------------------ #
#' Read items
#'
#' @param zotero_object `Zotero` object
#' @param collection Collection key. If specified, items within a specific collection will be retrieved.
#' @param top Specify item levels (default: `FALSE`)
#' * `FALSE`: All items in the library, excluding trashed items
#' * `TRUE `: \strong{Top-level} items in the library, excluding trashed items.
#' @param ... Query inputs. See \strong{Read Requests} and \strong{Sorting and Pagination} in the [Zotero Web API Documents](https://www.zotero.org/support/dev/web_api/v3/basics) for available query.
#' @param timeout Seconds for the timeout of request (default: `20`).
#'
#' @return `httr::response` object
#' @export
#'
#' @examples
#' \dontrun{
#' ztr <- Zotero$new("ztr4r")
#'
#' # simple example
#' ztr %>%
#'   ztr_read_items()
#'
#' # example with query: retrieve items
#' ztr %>%
#'   ztr_read_items(
#'     collection = NULL, top = TRUE,
#'     format = "json", sort = "dateAdded"
#'   )
#'
#' # example with query: retrieve items in a specific collection
#' ztr %>%
#'   ztr_read_items(
#'     collection = "foo", top = TRUE,
#'     format = "json", sort = "dateAdded"
#'   )
#' }
ztr_read_items <- function(zotero_object, collection = NULL, top = FALSE, ..., timeout = 20) {
  if (is.null(collection)) {
    path <- c("items", if (top) {"top"})
  } else {
    path <- c("collections", collection, "items", if (top) {"top"})
  }

  zotero_object$request(
    verb = "GET",
    path = path,
    ...,
    timeout = 20
  )
}


# ------------------------------ #
#          ztr_read_item         #
# ------------------------------ #
#' Read a specific item or child items within a specific item
#'
#' @param zotero_object `Zotero` object
#' @param item Item key
#' @param child Specify whether to retrieve child items within a specific item Otherwise, a specific item will be retrieved.
#' @param ... Query inputs. See \strong{Read Requests} and \strong{Sorting and Pagination} in the [Zotero Web API Documents](https://www.zotero.org/support/dev/web_api/v3/basics) for available query.
#' @param timeout Seconds for the timeout of request (default: `20`).
#'
#' @return `httr::response` object
#' @export
#'
#' @examples
#' \dontrun{
#' ztr <- Zotero$new("ztr4r")
#'
#' # simple example
#' ztr %>%
#'   ztr_read_item(item = "foo")
#'
#' # example with query
#' ztr %>%
#'   ztr_read_item(
#'     item = "foo", child = TRUE,
#'     format = "json", sort = "dateAdded"
#'   )
#' }
ztr_read_item <- function(zotero_object, item, child = FALSE, ..., timeout = 20) {
  stopifnot(
    is.character(item),
    stringr::str_length(item) > 0
  )

  zotero_object$request(
    verb = "GET",
    path = c("items", item, if (child) {"children"}),
    ...,
    timeout = 20
  )
}


# ------------------------------ #
#         ztr_read_trash         #
# ------------------------------ #
#' Read trashed items
#'
#' @param zotero_object `Zotero` object
#' @param ... Query inputs. See \strong{Read Requests} and \strong{Sorting and Pagination} in the [Zotero Web API Documents](https://www.zotero.org/support/dev/web_api/v3/basics) for available query.
#' @param timeout Seconds for the timeout of request (default: `20`).
#'
#' @return `httr::response` object
#' @export
#'
#' @examples
#' \dontrun{
#' ztr <- Zotero$new("ztr4r")
#'
#' # simple example
#' ztr %>%
#'   ztr_read_trash()
#'
#' # example with query
#' ztr %>%
#'   ztr_read_trash(
#'     format = "json", sort = "dateAdded"
#'   )
#' }
ztr_read_trash <- function(zotero_object, ..., timeout = 20) {
  zotero_object$request(
    verb = "GET",
    path = c("items", "trash"),
    ...,
    timeout = 20
  )
}


# ------------------------------ #
#      ztr_read_publications     #
# ------------------------------ #
#' Read items in My Publications
#'
#' @param zotero_object `Zotero` object
#' @param ... Query inputs. See \strong{Read Requests} and \strong{Sorting and Pagination} in the [Zotero Web API Documents](https://www.zotero.org/support/dev/web_api/v3/basics) for available query.
#' @param timeout Seconds for the timeout of request (default: `20`).
#'
#' @return `httr::response` object
#' @export
#'
#' @examples
#' \dontrun{
#' ztr <- Zotero$new("ztr4r")
#'
#' # simple example
#' ztr %>%
#'   ztr_read_publications()
#'
#' # example with query
#' ztr %>%
#'   ztr_read_publications(
#'     format = "json", sort = "dateAdded"
#'   )
#' }
ztr_read_publications <- function(zotero_object, ..., timeout = 20) {
  zotero_object$request(
    verb = "GET",
    path = c("publications", "items"),
    ...,
    timeout = 20
  )
}


# ------------------------------ #
#          ztr_read_tags         #
# ------------------------------ #
#' Read all tags in the library or tags of all types matching a specific name
#'
#' @param zotero_object `Zotero` object
#' @param tags tag searching pattern
#' @param ... Query inputs. See \strong{Read Requests} and \strong{Sorting and Pagination} in the [Zotero Web API Documents](https://www.zotero.org/support/dev/web_api/v3/basics) for available query.
#' @param timeout Seconds for the timeout of request (default: `20`).
#'
#' @return `httr::response` object
#' @export
#'
#' @examples
#' \dontrun{
#' ztr <- Zotero$new("ztr4r")
#'
#' # simple example
#' ztr %>%
#'   ztr_read_tags(tags = "tag=foo")
#'
#' # example with query
#' ztr %>%
#'   ztr_read_tags(
#'     tags = "tag=foo bar"
#'     format = "json", sort = "dateAdded"
#'   )
#' }
ztr_read_tags <- function(zotero_object, tags = NULL, ..., timeout = 20) {
  zotero_object$request(
    verb = "GET",
    path = c("tags", if (!is.null(tags)) {tags}),
    ...,
    timeout = 20
  )
}


# ------------------------------ #
#    ztr_read_collection_tags    #
# ------------------------------ #
#' Read tags within a specific collection in the library
#'
#' @param zotero_object `Zotero` object
#' @param collection Collection key
#' @param ... Query inputs. See \strong{Read Requests} and \strong{Sorting and Pagination} in the [Zotero Web API Documents](https://www.zotero.org/support/dev/web_api/v3/basics) for available query.
#' @param timeout Seconds for the timeout of request (default: `20`).
#'
#' @return `httr::response` object
#' @export
#'
#' @examples
#' \dontrun{
#' ztr <- Zotero$new("ztr4r")
#'
#' # simple example
#' ztr %>%
#'   ztr_read_collection_tags(collection = "foo")
#'
#' # example with query
#' ztr %>%
#'   ztr_read_collection_tags(
#'     collection = "foo",
#'     format = "json", sort = "dateAdded"
#'   )
#' }
ztr_read_collection_tags <- function(zotero_object, collection, ..., timeout = 20) {
  stopifnot(
    is.character(collection),
    stringr::str_length(collection) > 0
  )

  zotero_object$request(
    verb = "GET",
    path = c("collections", collection, "tags"),
    ...,
    timeout = 20
  )
}


# ------------------------------ #
#       ztr_read_items_tags      #
# ------------------------------ #
#' Read all tags assigned to items in the library or tags assigned to top-level items
#'
#' @param zotero_object `Zotero` object
#' @param collection Collection key. Tags within a specific collection in the library will be retrieved.
#' @param top Specify item levels (default: `FALSE`)
#' * `FALSE`: All tags in the library, with the ability to filter based on the items
#' * `TRUE `: Tags assigned to top-level items
#' @param ... Query inputs. See \strong{Read Requests} and \strong{Sorting and Pagination} in the [Zotero Web API Documents](https://www.zotero.org/support/dev/web_api/v3/basics) for available query.
#' @param timeout Seconds for the timeout of request (default: `20`).
#'
#' @return `httr::response` object
#' @export
#'
#' @examples
#' \dontrun{
#' ztr <- Zotero$new("ztr4r")
#'
#' # simple example
#' ztr %>%
#'   ztr_read_items_tags()
#'
#' # example with query: retrieve items
#' ztr %>%
#'   ztr_read_items_tags(
#'     collection = NULL, top = TRUE,
#'     format = "json", sort = "dateAdded"
#'   )
#'
#' # example with query: retrieve items in a specific collection
#' ztr %>%
#'   ztr_read_items_tags(
#'     collection = "foo", top = TRUE,
#'     format = "json", sort = "dateAdded"
#'   )
#' }
ztr_read_items_tags <- function(zotero_object, collection = NULL, top = FALSE, ..., timeout = 20) {
  if (is.null(collection)) {
    path <- c("items", if (top) {"top"}, "tags")
  } else {
    path <- c("collections", collection, "items", if (top) {"top"}, "tags")
  }

  zotero_object$request(
    verb = "GET",
    path = path,
    ...,
    timeout = 20
  )
}


# ------------------------------ #
#        ztr_read_item_tags      #
# ------------------------------ #
#' Read tags associated with a specific item
#'
#' @param zotero_object `Zotero` object
#' @param item Item key
#' @param ... Query inputs. See \strong{Read Requests} and \strong{Sorting and Pagination} in the [Zotero Web API Documents](https://www.zotero.org/support/dev/web_api/v3/basics) for available query.
#' @param timeout Seconds for the timeout of request (default: `20`).
#'
#' @return `httr::response` object
#' @export
#'
#' @examples
#' \dontrun{
#' ztr <- Zotero$new("ztr4r")
#'
#' # simple example
#' ztr %>%
#'   ztr_read_item_tags(item = "foo")
#'
#' # example with query
#' ztr %>%
#'   ztr_read_item_tags(
#'     item = "foo",
#'     format = "json", sort = "dateAdded"
#'   )
#' }
ztr_read_item_tags <- function(zotero_object, item, ..., timeout = 20) {
  stopifnot(
    is.character(item),
    stringr::str_length(item) > 0
  )

  zotero_object$request(
    verb = "GET",
    path = c("items", item, "tags"),
    ...,
    timeout = 20
  )
}


# ------------------------------ #
#       ztr_read_trash_tags      #
# ------------------------------ #
#' Read tags assigned to items in the trash
#'
#' @param zotero_object `Zotero` object
#' @param ... Query inputs. See \strong{Read Requests} and \strong{Sorting and Pagination} in the [Zotero Web API Documents](https://www.zotero.org/support/dev/web_api/v3/basics) for available query.
#' @param timeout Seconds for the timeout of request (default: `20`).
#'
#' @return `httr::response` object
#' @export
#'
#' @examples
#' \dontrun{
#' ztr <- Zotero$new("ztr4r")
#'
#' # simple example
#' ztr %>%
#'   ztr_read_trash_tags()
#'
#' # example with query
#' ztr %>%
#'   ztr_read_trash_tags(
#'     format = "json", sort = "dateAdded"
#'   )
#' }
ztr_read_trash_tags <- function(zotero_object, ..., timeout = 20) {
  zotero_object$request(
    verb = "GET",
    path = c("items", "trash", "tags"),
    ...,
    timeout = 20
  )
}


# ------------------------------ #
#   ztr_read_publications_tags   #
# ------------------------------ #
#' Read tags assigned to items in My Publications
#'
#' @param zotero_object `Zotero` object
#' @param ... Query inputs. See \strong{Read Requests} and \strong{Sorting and Pagination} in the [Zotero Web API Documents](https://www.zotero.org/support/dev/web_api/v3/basics) for available query.
#' @param timeout Seconds for the timeout of request (default: `20`).
#'
#' @return `httr::response` object
#' @export
#'
#' @examples
#' \dontrun{
#' ztr <- Zotero$new("ztr4r")
#'
#' # simple example
#' ztr %>%
#'   ztr_read_publications_tags()
#'
#' # example with query
#' ztr %>%
#'   ztr_read_publications_tags(
#'     format = "json", sort = "dateAdded"
#'   )
#' }
ztr_read_publications_tags <- function(zotero_object, ..., timeout = 20) {
  zotero_object$request(
    verb = "GET",
    path = c("publications", "items", "tags"),
    ...,
    timeout = 20
  )
}

