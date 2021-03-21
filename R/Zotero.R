# ------------------------------ #
#       Zotero Class Object      #
# ------------------------------ #
#' Zotero Class Object
#'
#' @description `Zotero` object stores
#' \itemize{
#' \item user information
#' \item cached requests
#' } and provides
#' \itemize{
#' \item `print` method
#' \item `request` method
#' }
#' @details The `request` method are wrapped with
#' \itemize{
#' \item `ztr_read_*`
#' \item `ztr_write_*`
#' \item `ztr_search_*`
#' }
#' @export
Zotero <- R6::R6Class("Zotero",

  public = list(
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
      private$name = auth$name
      private$id = auth$id
      private$library_type = auth$library_type
      private$privacy = auth$privacy
      private$api_key = auth$api_key
    },

    #' @description Print a `Zotero` object
    #' @return None
    print = function() {
      print(
        stringr::str_glue(
          "
          <Zotero Web API>
              Key Name: {private$name}
               Privacy: {private$privacy}
                  Type: {private$library_type}
                    ID: {if (private$privacy == 'private') {'<hidden>'} else {NA}}
               API Key: {if (private$privacy == 'private') {'<hidden>'} else {NA}}
          "
        )
      )
      invisible(self)
    },

    #' @description Send request
    #' @details This function is not supposed to be called directory. Instead, please use other specific functions. For available values for `path` and `...` (query), please see <https://www.zotero.org/support/dev/web_api/v3/basics>
    #' @param verb Request verb. Either `"GET"` or `"POST"`.
    #' @param path Request path.
    #' @param ... Query inputs.
    #' @param timeout Seconds for the timeout of request.
    request = function(verb, path, ..., timeout = 20) {
      # validate inputs
      stopifnot(
        verb %in% c("GET","POST"),
        is.numeric(timeout)
      )
      # send request
      response <- httr::VERB(
        verb = verb,
        url = httr::modify_url(url = "https://api.zotero.org",
                               path = c(private$library_type, private$id, path),
                               query = rlang::dots_list(...)),
        httr::add_headers("Zotero-API-Version" = get_api_version(),
                          "Zotero-API-Key" = private$api_key),
        httr::timeout(seconds = as.integer(timeout)),
        httr::user_agent(agent = "https://github.com/tellnnn/ztr4r")
      )
      # add cache
      private$cache_request(response)
      # check response
      httr::stop_for_status(response)
      # return response
      return(response)
    }
  ),

  private = list(
    name = NULL,
    id = NULL,
    library_type = NULL,
    privacy = NULL,
    api_key = NULL,
    cache_request = function(response) {
      self$cache <-
        self$cache %>%
        tibble::add_row(
          timestamp = lubridate::with_tz(response$date, tz = Sys.timezone()),
          url = response$url,
          status = httr::status_code(response),
          last_modified_version = response$headers$`last-modified-version`,
          content_type = response$headers$`content-type`
        )
    }
  ),
  lock_objects = TRUE,
  lock_class = TRUE,
  cloneable = FALSE
)
