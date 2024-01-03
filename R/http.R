httpGET <- function(url, as = NULL, ...) {
	result <- httr::GET(url, ...)
	httr::stop_for_status(result)
	httr::content(result, as = as)
}

httpPOST <- function(url, as = NULL, ...) {
	result <- httr::POST(url, ...)
	httr::stop_for_status(result)
	httr::content(result, as = as)
}

#' @importFrom fs path
make_url <- function(...) {
	base_url <- Sys.getenv('CROMWELLURL')
	file.path(base_url, ...)
}
