#' Look for Cromwell URL as the env var CROMWELLURL
#' @export
#' @return a string, empty if env var not found
cw_url <- function() {
  Sys.getenv("CROMWELLURL")
}

#' @importFrom rlang has_name
error_body <- function(response) {
  parsed <- resp_body_json(response)
  mssg <- if (rlang::has_name(parsed, "error")) {
    parsed$error
  } else if (rlang::has_name(parsed, "message")) {
    parsed$message
  } else {
    "None"
  }
  glue::glue("Additional context: {mssg}")
}

http_req_get <- function(url, token = NULL) {
  request(url) |>
    try_auth_header(token) |>
    req_error(body = error_body)
}

http_req_post <- function(url, token = NULL) {
  request(url) |>
    req_method("POST") |>
    try_auth_header(token) |>
    req_error(body = error_body)
}

http_perform <- function(request) {
  req_perform(request) |>
    resp_body_json()
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
#' @param .req An `httr2` request
#' @param token (character) a Bearer token. optional. if nothing
#' passed, we look for the env var PROOF_TOKEN
#' @keywords internal
#' @return An `httr2_request` S3 class adding an HTTP header for
#' `Authorization` with the value in `token`
try_auth_header <- function(.req, token = NULL) {
  token <- token_finder(token)
  if (nzchar(token)) {
    req_headers(.req, Authorization = paste0("Bearer ", token))
  } else {
    .req
  }
}
