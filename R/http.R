#' @importFrom httr GET POST stop_for_status content upload_file
#' @noRd
#' @keywords internal
#' @author Scott Chamberlain
http_get <- function(url, as = NULL, ...) {
  result <- httr::GET(url, try_proof_header(), ...)
  httr::stop_for_status(result)
  httr::content(result, as = as)
}

http_post <- function(url, as = NULL, ...) {
  result <- httr::POST(url, try_proof_header(), ...)
  httr::stop_for_status(result)
  httr::content(result, as = as)
}

make_url <- function(...) {
  base_url <- Sys.getenv("CROMWELLURL")
  cleaned_url <- gsub("/+$", "", base_url)
  file.path(cleaned_url, ...)
}

check_url <- function() {
  x <- Sys.getenv("CROMWELLURL")
  if (identical(x, "")) {
    stop("Set the env var `CROMWELLURL`. ",
      "See ?cromwell_settings",
      call. = FALSE
    )
  }
}

#' Get header for PROOF API calls
#'
#' @keywords internal
#' @return A `request` S3 class with the HTTP header that can be passed
#' to `httr::GET()`, `httr::POST()`, etc.
try_proof_header <- function() {
  token <- Sys.getenv("PROOF_TOKEN", "")
  if (nzchar(token) && nchar(token) > 30) {
    httr::add_headers(Authorization = paste0("Bearer ", token))
  }
}
