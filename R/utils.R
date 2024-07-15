#' @importFrom curl form_file
lst_upload_file <- function(path) {
  list(curl::form_file(path))
}

# copy with changes of code in `httr::BROWSE`
browse <- function(url) {
  if (interactive()) {
    utils::browseURL(url)
  }
  else {
    message("Please point your browser to the following url: ")
    message(url)
  }
}
