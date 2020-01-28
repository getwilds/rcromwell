
#' Get the version of a Cromwell server
#'
#'
#' @return Cromwell version
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell server URL to be set in the environment. (use `setCromwellURL()`)
#' @examples
#' TBD
#' @export
cromwellVersion <- function() {
  if ("" %in% Sys.getenv("CROMWELLURL")) {
    stop("CROMWELLURL is not set.")
  }  else
    message("Getting timing diagram from Cromwell.")
  httr::content(httr::GET(paste0(
    Sys.getenv("CROMWELLURL"),
    "/engine/v1/version"
  )))
}
