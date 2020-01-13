#' Get the available backends of a Cromwell server
#'
#'
#' @return Cromwell backend options
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell URL to be set in the environment.
#' @export
cromwellBackends <- function() {
  if ("" %in% Sys.getenv("CROMWELLURL")) {
    stop("CROMWELLURL is not set.")
  }  else
    print("Getting backend options from Cromwell.")
  httr::content(httr::GET(paste0(
    Sys.getenv("CROMWELLURL"),
    "/api/workflows/v1/backends"
  )))
}
