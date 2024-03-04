#' Look for Cromwell URL as the env var CROMWELLURL
#' @export
#' @return a string, empty if env var not found
cw_url <- function() {
  Sys.getenv("CROMWELLURL")
}

con_utf8 <- function(response, as = NULL, ...) {
  httr::content(response, as = as, encoding = "utf-8", ...)
}

#' @importFrom rlang has_name
#' @importFrom httr status_code
#' @importFrom rlang abort caller_env
handle_error <- function(response, call = caller_env()) {
  status <- httr::status_code(response)
  if (status >= 400) {
    err <- con_utf8(response, as = "parsed")
    if (rlang::has_name(err, "error")) {
      mssg <- err$error
    } else if (rlang::has_name(err, "message")) {
      mssg <- err$message
    } else {
      httr::stop_for_status(response)
    }
    abort(
      sprintf("(HTTP %s) - %s", as.character(status), mssg),
      call = call
    )
  }
}

#' @importFrom httr GET POST stop_for_status content upload_file
#' @noRd
#' @keywords internal
#' @author Scott Chamberlain
http_get <- function(url, as = NULL, token = NULL, call = caller_env(), ...) {
  result <- httr::GET(url, try_auth_header(token), ...)
  handle_error(result, call)
  con_utf8(result, as = as)
}

http_post <- function(url, as = NULL, token = NULL, call = caller_env(), ...) {
  result <- httr::POST(url, try_auth_header(token), ...)
  handle_error(result, call)
  con_utf8(result, as = as)
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
