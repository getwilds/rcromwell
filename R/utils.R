lst_upload_file <- function(path) {
  list(httr::upload_file(path))
}
