
#' Get the version of a Cromwell server
#'
#'
#' @return Cromwell version
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell URL to be set in the environment.
#' @examples
#' TBD
#' @export
cromwellVersion <- function() {
  if ("" %in% Sys.getenv("CROMWELLURL")) {
    stop("CROMWELLURL is not set.")
  }  else
    print("Getting timing diagram from Cromwell.")
  httr::content(httr::GET(paste0(
    Sys.getenv("CROMWELLURL"),
    "/engine/v1/version"
  )))
}