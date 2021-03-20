# ------------------------------ #
#             request            #
# ------------------------------ #
#' Send request
#'
#' Send request to Zotero Web API.
#'
#' @param name A `character` string. Name of Zotero Web API key.
#' @param verb A `character` string. Either `"GET"` or `"POST"` depending on the request.
#' @param url A `character` string. URL to be set.
#' @param item A `logical`. Specify whether item request or not.
#' @param ... Query inputs. For valid queries, please see `validate_query()`.
#' @param timeout An `integer`. Number of seconds to wait for a response until giving up.
#'
#' @return A `response()` object
#'
#' @examples
#' \dontrun{
#' request(name = "your_api_key_name", verb = "GET", url = "url", item = TRUE)
#' }
request <- function(name, verb, url, item, ..., timeout = 20) {
  # validate inputs
  stopifnot(
    stringr::str_length(name) > 0,
    verb %in% c("GET","POST"),
    stringr::str_length(url) > 0,
    is.logical(item),
    is.integer(timeout)
  )

  # send request
  response <- httr::VERB(
    verb = verb,
    url = url,
    httr::set_headers(
      "Zotero-API-Version" = get_api_version(),
      "Zotero-API-Key" = get_api_key(name = name)
    ),
    httr::timeout(seconds = timeout),
    httr::user_agent(agent = "https://github.com/tellnnn/ztr4r"),
    query = rlang::dots_list(...)
  )

  # check response
  httr::stop_for_status(response)

  # return response
  return(response)
}

