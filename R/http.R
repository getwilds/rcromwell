#' Look for Cromwell URL as the env var CROMWELLURL
#' @export
#' @return a string, empty if env var not found
cw_url <- function() {
  Sys.getenv("CROMWELLURL")
}

#' @importFrom httr GET POST stop_for_status content upload_file
#' @noRd
#' @keywords internal
#' @author Scott Chamberlain
http_get <- function(url, as = NULL, token = NULL, ...) {
  result <- httr::GET(url, try_auth_header(token), ...)
  httr::stop_for_status(result)
  httr::content(result, as = as)
}

http_post <- function(url, as = NULL, token = NULL, ...) {
  result <- httr::POST(url, try_auth_header(token), ...)
  httr::stop_for_status(result)
  httr::content(result, as = as)
}

make_url <- function(base_url, ...) {
  rlang::is_character(base_url)
  cleaned_url <- gsub("/+$", "", base_url)
  file.path(cleaned_url, ...)
}

#' Check a url
#' @param x a url. default: NULL
#' @keywords internal
check_url <- function(x = NULL) {

  if (identical(x, "") || is.null(x)) {
    stop("Set the env var `CROMWELLURL`. ",
      "See ?cromwell_settings",
      call. = FALSE
    )
  }
}

token_finder <- function(token = NULL) {
  if (!is.null(token)) {
    return(token)
  }
  Sys.getenv("PROOF_TOKEN")
}

#' Add Authorization header
#'
#' @param token (character) a Bearer token. optional. if nothing
#' passed, we look for the env var PROOF_TOKEN
#' @keywords internal
#' @return A `request` S3 class with the HTTP header that can be passed
#' to `httr::GET()`, `httr::POST()`, etc.
try_auth_header <- function(token = NULL) {
  token <- token_finder(token)
  if (nzchar(token)) {
    httr::add_headers(Authorization = paste0("Bearer ", token))
  }
}
