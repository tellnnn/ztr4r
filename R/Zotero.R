# ------------------------------ #
#       Zotero Class Object      #
# ------------------------------ #
#' Zotero Class Object
#'
#' @description `Zotero` object
#' stores (1) user information and (2) cached request table,
#' and provides (1) `print` and (2) `request` method
#'
#' @details The `request` method are wrapped with
#' \itemize{
#' \item `ztr_read_*`
#' \item `ztr_write_*`
#' }
#' @export
Zotero <- R6::R6Class("Zotero",

  public = list(
    #' @field name Name of Zotero Web API Key
    name = NULL,

    #' @field library_type Library Type. `"users"` or `"groups"`
    library_type = NULL,

    #' @field privacy Privacy Setting of the library. `"private"` or `"public"`
    privacy = NULL,

    #' @field cache Cached request table
    cache = tibble::tibble(
      timestamp = lubridate::NA_POSIXct_,
      url = NA_character_,
      status = NA_integer_,
      last_modified_version = NA_character_,
      content_type = NA_character_,
      .rows = 0
    ),

    #' @description Create a new `Zotero` object
    #' @param name Name of Zotero Web API key
    #' @return A new `Zotero` object
    initialize = function(name) {
      auth <- load_api_key(name = name) %||% create_api_key(name = name)
      self$name = auth$name
      private$id = auth$id
      self$library_type = auth$library_type
      self$privacy = auth$privacy
      private$api_key = auth$api_key
    },

    #' @description Print a `Zotero` object
    #' @return None
    print = function() {
      print(
        stringr::str_glue(
          "
          <Zotero Web API>
              Key Name: {self$name}
               Privacy: {self$privacy}
                  Type: {self$library_type}
                    ID: {if (self$privacy == 'private') {'<hidden>'} else {NA}}
               API Key: {if (self$privacy == 'private') {'<hidden>'} else {NA}}
          "
        )
      )
      invisible(self)
    },

    #' @description Send request
    #' @details This function is not supposed to be called directory.
    #' Instead, please use other specific functions.
    #' For available values for `path` and `...` (query),
    #' please see <https://www.zotero.org/support/dev/web_api/v3/basics>
    #'
    #' @param verb Request verb. Either `"GET"` or `"POST"`
    #' @param path Request path
    #' @param key Flag to indicate whether read privileges or not
    #' @param ... Query inputs
    #' @param timeout Seconds for the timeout of request
    request = function(verb, path, key, ..., timeout) {
      # validate inputs
      stopifnot(
        verb %in% c("GET","POST"),
        is.numeric(timeout)
      )

      if (key) {
        url <- httr::modify_url(
          url = "https://api.zotero.org",
          path = c("keys", private$api_key)
        )
      } else {
        url <- httr::modify_url(
          url = "https://api.zotero.org",
          path = c(self$library_type, private$id, path),
          query = rlang::dots_list(...)
        )
      }

      # send request
      response <- httr::VERB(
        verb = verb,
        url = url,
        httr::add_headers(
          "Zotero-API-Version" = get_api_version(),
          "Zotero-API-Key" = private$api_key
        ),
        httr::timeout(seconds = as.integer(timeout)),
        httr::user_agent(agent = "https://github.com/tellnnn/ztr4r")
      )
      # check response
      httr::stop_for_status(response)
      # add cache
      private$cache_request(response)
      # return response
      return(response)
    }
  ),

  private = list(
    # private field
    id = NULL,
    api_key = NULL,
    # private function
    cache_request = function(response) {
      self$cache <-
        self$cache %>%
        tibble::add_row(
          timestamp = response$date,
          url = response$url,
          status = httr::status_code(response),
          last_modified_version = response$headers$`last-modified-version` %||% NA_character_,
          content_type = response$headers$`content-type` %||% NA_character_
        )
    }
  ),
  lock_objects = TRUE,
  lock_class = TRUE,
  cloneable = FALSE
)
