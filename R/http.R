#' @importFrom httr GET POST stop_for_status content upload_file
#' @noRd
#' @keywords internal
http_get <- function(url, as = NULL, ...) {
  result <- httr::GET(url, ...)
  httr::stop_for_status(result)
  httr::content(result, as = as)
}

http_post <- function(url, as = NULL, ...) {
  result <- httr::POST(url, ...)
  httr::stop_for_status(result)
  httr::content(result, as = as)
}

make_url <- function(...) {
  base_url <- Sys.getenv("CROMWELLURL")
  file.path(base_url, ...)
}

check_url <- function() {
  x <- Sys.getenv("CROMWELLURL")
  if (identical(x, "")) {
    stop("Set the env var `CROMWELLURL`. ",
      "See ?cromwellSettings",
      call. = FALSE
    )
  }
}
