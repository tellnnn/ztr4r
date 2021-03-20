Zotero <- R6::R6Class("Zotero",
  public = list(
    cache = NULL,
    initialize = function(name) {
      auth <- load_api_key(name = name) %||% create_api_key(name = name)
      private$name = auth$name
      private$id = auth$id
      private$library_type = auth$library_type
      private$privacy = auth$privacy
      private$api_key = auth$api_key
    },
    print = function(...) {
      cat("<Zotero Web API>\n")
      cat("  Key Name: ", private$name, "\n", sep = "")
      cat("   Privacy: ", private$privacy, "\n", sep = "")
      cat("      Type: ", private$library_type, "\n", sep = "")
      if (private$privacy == "public") {
        cat("        ID: ", private$id, "\n", sep = "")
        cat("   API Key: ", private$api_key, "\n", sep = "")
      } else {
        cat("        ID: <hidden>\n")
        cat("   API Key: <hidden>\n")
      }
      cat("     cache:\n")
      print(head(self$cache))
      invisible(self)
    },
    request = function(verb, path, ..., timeout = 20) {
      # validate inputs
      stopifnot(
        verb %in% c("GET","POST"),
        is.integer(timeout)
      )

      # send request
      response <- httr::VERB(
        verb = verb,
        url = httr::modify_url(
          url = "https://api.zotero.org",
          path = c(private$library_type, private$id, path),
          query = rlang::dots_list(...)
        ),
        httr::add_headers(
          "Zotero-API-Version" = get_api_version(),
          "Zotero-API-Key" = private$api_key
        ),
        httr::timeout(seconds = timeout),
        httr::user_agent(agent = "https://github.com/tellnnn/ztr4r")
      )

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
    api_key = NULL
  ),
  lock_class = TRUE
)
