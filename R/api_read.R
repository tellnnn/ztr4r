# The implementations basically follows the structure of Zotero Web API Documentation <https://www.zotero.org/support/dev/web_api/v3/basics#search_syntax>


# Collections ------------------------------

# ------------------------------ #
#      ztr_read_collections      #
# ------------------------------ #
#' Read (Top-Level) Collections In The Library
#'
#' @param zotero_object `Zotero` object.
#' @param top Collection level (default: `FALSE`):
#' * `FALSE`: All the collections in the library
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
#'     format = "json", sort = "dateModified", direction = "asc", limit = 25, start = 0
#'   )
#' }
ztr_read_collections <- function(zotero_object, top = FALSE, ..., timeout = 20) {
  zotero_object$request(
    verb = "GET",
    path = c("collections", if (top) {"top"}),
    key = FALSE,
    ...,
    timeout = 20
  )
}


# ------------------------------ #
#      ztr_read_collection       #
# ------------------------------ #
#' Read A Specific Collection Or Sub-Collections Within A Specific Collection
#'
#' @param zotero_object `Zotero` object.
#' @param collection Collection key.
#' @param sub Specify whether to retrieve sub-collections within a specific collection. Otherwise, a specified collection will be retrieved.
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
#' # example with query: sub-collections within a specific collection
#' ztr %>%
#'   ztr_read_collection(
#'     collection = "foo", sub = TRUE,
#'     format = "json", sort = "dateModified", direction = "asc", limit = 25, start = 0
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
    key = FALSE,
    ...,
    timeout = 20
  )
}




# Items ------------------------------

# ------------------------------ #
#         ztr_read_items         #
# ------------------------------ #
#' Read (Top-Level) Items In The Library, Excluding Trashed Items
#'
#' @param zotero_object `Zotero` object.
#' @param collection Collection key. If specified, items within a specific collection will be retrieved.
#' @param top Item level (default: `FALSE`):
#' * `FALSE`: All the items in the library, excluding trashed items
#' * `TRUE `: \strong{Top-level} items in the library, excluding trashed items
#' @param ... Query inputs. See \strong{Read Requests}, \strong{Searching}, and \strong{Sorting and Pagination} in the [Zotero Web API Documents](https://www.zotero.org/support/dev/web_api/v3/basics) for available query.
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
#' # example with query: All the items in the library, excluding trashed items
#' ztr %>%
#'   ztr_read_items(
#'     collection = NULL, top = TRUE,
#'     format = "json", sort = "dateModified", direction = "asc", limit = 25, start = 0
#'   )
#'
#' # example with query: Top-level items in the library, excluding trashed items.
#' ztr %>%
#'   ztr_read_items(
#'     collection = "foo", top = TRUE,
#'     format = "json", sort = "dateModified", direction = "asc", limit = 25, start = 0
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
    key = FALSE,
    ...,
    timeout = 20
  )
}


# ------------------------------ #
#          ztr_read_item         #
# ------------------------------ #
#' Read A Specific Item Or Child Items Under A Specific Item
#'
#' @param zotero_object `Zotero` object.
#' @param item Item key.
#' @param child Specify whether to retrieve child items under a specific item. Otherwise, a specific item will be retrieved.
#' @param ... Query inputs. See \strong{Read Requests}, \strong{Searching}, and \strong{Sorting and Pagination} in the [Zotero Web API Documents](https://www.zotero.org/support/dev/web_api/v3/basics) for available query.
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
#' # example with query: child items under a specific item
#' ztr %>%
#'   ztr_read_item(
#'     item = "foo", child = TRUE,
#'     format = "json", sort = "dateModified", direction = "asc", limit = 25, start = 0
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
    key = FALSE,
    ...,
    timeout = 20
  )
}


# ------------------------------ #
#         ztr_read_trash         #
# ------------------------------ #
#' Read Items In The Trash
#'
#' @param zotero_object `Zotero` object.
#' @param ... Query inputs. See \strong{Read Requests}, \strong{Searching}, and \strong{Sorting and Pagination} in the [Zotero Web API Documents](https://www.zotero.org/support/dev/web_api/v3/basics) for available query.
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
#'     format = "json", sort = "dateModified", direction = "asc", limit = 25, start = 0
#'   )
#' }
ztr_read_trash <- function(zotero_object, ..., timeout = 20) {
  zotero_object$request(
    verb = "GET",
    path = c("items", "trash"),
    key = FALSE,
    ...,
    timeout = 20
  )
}


# ------------------------------ #
#      ztr_read_publications     #
# ------------------------------ #
#' Read Items In My Publications
#'
#' @param zotero_object `Zotero` object.
#' @param ... Query inputs. See \strong{Read Requests}, \strong{Searching}, and \strong{Sorting and Pagination} in the [Zotero Web API Documents](https://www.zotero.org/support/dev/web_api/v3/basics) for available query.
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
#'     format = "json", sort = "dateModified", direction = "asc", limit = 25, start = 0
#'   )
#' }
ztr_read_publications <- function(zotero_object, ..., timeout = 20) {
  zotero_object$request(
    verb = "GET",
    path = c("publications", "items"),
    key = FALSE,
    ...,
    timeout = 20
  )
}




# Searches ------------------------------

# ------------------------------ #
#        ztr_read_searches       #
# ------------------------------ #
#' Read All Saved Searches In The Library
#'
#' @param zotero_object `Zotero` object.
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
#'   ztr_read_searches()
#' }
ztr_read_searches <- function(zotero_object, timeout = 20) {
  zotero_object$request(
    verb = "GET",
    path = "searches",
    key = FALSE,
    timeout = 20
  )
}


# ------------------------------ #
#        ztr_read_search         #
# ------------------------------ #
#' Read A Specific Saved Search In The Library
#'
#' @param zotero_object `Zotero` object.
#' @param search Search key.
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
#'   ztr_read_search()
#' }
ztr_read_search <- function(zotero_object, search, timeout = 20) {
  zotero_object$request(
    verb = "GET",
    path = "searches",
    key = FALSE,
    timeout = 20
  )
}




# Tags ------------------------------

# ------------------------------ #
#          ztr_read_tags         #
# ------------------------------ #
#' Read All Tags In The Library Or Tags Of All Types Matching A Specific Name
#'
#' @param zotero_object `Zotero` object.
#' @param tag URL encoded tag
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
#'   ztr_read_tags(tag = "tag=foo")
#'
#' # example with query
#' ztr %>%
#'   ztr_read_tags(
#'     tag = "foo",
#'     format = "json", limit = 25
#'   )
#' }
ztr_read_tags <- function(zotero_object, tag = NULL, ..., timeout = 20) {
  zotero_object$request(
    verb = "GET",
    path = c("tags", if (!is.null(tag)) {tag}),
    key = FALSE,
    ...,
    timeout = 20
  )
}


# ------------------------------ #
#    ztr_read_collection_tags    #
# ------------------------------ #
#' Read Tags Within A Specific Collection In The Library
#'
#' @param zotero_object `Zotero` object.
#' @param collection Collection key.
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
#'     format = "json", limit = 25
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
    key = FALSE,
    ...,
    timeout = 20
  )
}


# ------------------------------ #
#       ztr_read_items_tags      #
# ------------------------------ #
#' Read Tags Assigned To Items (In A Given Collection)
#'
#' Read
#' 1. All Tags In The Library, With The Ability To Filter Based On The Items
#' 2. Tags Assigned To Top-Level Items
#' 3. Tags Assigned To Items In A Given Collection
#' 4. Tags Assigned To Top-Level Items In A Given Collection
#'
#' @param zotero_object `Zotero` object.
#' @param collection Collection key (default: `NULL`).
#' @param top Item level (default: `FALSE`):
#' * `FALSE`: All tags assigned to the items in the library/specific collection
#' * `TRUE `: All tags assigned to the \strong{top-level} items in the library/specific collection
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
#' # example with query: all tags in the library
#' ztr %>%
#'   ztr_read_items_tags(
#'     collection = NULL, top = FALSE,
#'     format = "json", limit = 25
#'   )
#'
#' # example with query: tags assigned to top-level items
#' ztr %>%
#'   ztr_read_items_tags(
#'     collection = NULL, top = TRUE,
#'     format = "json", limit = 25
#'   )
#'
#' # example with query: tags assigned to items in a given collection
#' ztr %>%
#'   ztr_read_items_tags(
#'     collection = "foo", top = FALSE,
#'     format = "json", limit = 25
#'   )
#'
#'
#' # example with query: tags assigned to top-level items in a given collection
#' ztr %>%
#'   ztr_read_items_tags(
#'     collection = "foo", top = TRUE,
#'     format = "json", limit = 25
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
    key = FALSE,
    ...,
    timeout = 20
  )
}


# ------------------------------ #
#        ztr_read_item_tags      #
# ------------------------------ #
#' Read Tags Associated With A Specific Item
#'
#' @param zotero_object `Zotero` object.
#' @param item Item key.
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
#'     format = "json", limit = 25
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
    key = FALSE,
    ...,
    timeout = 20
  )
}


# ------------------------------ #
#       ztr_read_trash_tags      #
# ------------------------------ #
#' Read Tags Assigned To Items In The Trash
#'
#' @param zotero_object `Zotero` object.
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
#'     format = "json", limit = 25
#'   )
#' }
ztr_read_trash_tags <- function(zotero_object, ..., timeout = 20) {
  zotero_object$request(
    verb = "GET",
    path = c("items", "trash", "tags"),
    key = FALSE,
    ...,
    timeout = 20
  )
}


# ------------------------------ #
#   ztr_read_publications_tags   #
# ------------------------------ #
#' Read Tags Assigned To Items In My Publications
#'
#' @param zotero_object `Zotero` object.
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
#'     format = "json", limit = 25
#'   )
#' }
ztr_read_publications_tags <- function(zotero_object, ..., timeout = 20) {
  zotero_object$request(
    verb = "GET",
    path = c("publications", "items", "tags"),
    key = FALSE,
    ...,
    timeout = 20
  )
}




# Other URLs ------------------------------

# ------------------------------ #
#       ztr_read_privileges      #
# ------------------------------ #
#' Read The User Id And Privileges Of The Given Api Key
#'
#' @param zotero_object `Zotero` object.
#' @param timeout Seconds for the timeout of request (default: `20`).
#'
#' @return `httr::response` object
#' @export
#'
#' @examples
#' \dontrun{
#' ztr <- Zotero$new("ztr4r")
#' ztr %>%
#'   ztr_read_privileges()
#' }
ztr_read_privileges <- function(zotero_object, timeout = 20) {
  stopifnot(zotero_object$privacy == "private")

  zotero_object$request(
    verb = "GET",
    path = NULL,
    key = TRUE,
    timeout = timeout
  )
}


# ------------------------------ #
#         ztr_read_groups        #
# ------------------------------ #
#' Read The Set Of Groups The Current Api Key Has Access To
#'
#' @param zotero_object `Zotero` object.
#' @param timeout Seconds for the timeout of request (default: `20`).
#'
#' @return `httr::response` object
#' @export
#'
#' @examples
#' \dontrun{
#' ztr <- Zotero$new("ztr4r")
#' ztr %>%
#'   ztr_read_groups()
#' }
ztr_read_groups <- function(zotero_object, timeout = 20) {
  stopifnot(zotero_object$library_type == "users")

  zotero_object$request(
    verb = "GET",
    path = "groups",
    key = FALSE,
    timeout = timeout
  )
}
