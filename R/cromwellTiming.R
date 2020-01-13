#' Get a timing diagram for a Cromwell workflow
#'
#'
#' @param workflow_id Unique workflow id of the job.
#' @return Opens a timing diagram in a browser
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell URL to be set in the environment.
#' @examples
#' TBD
#' @export
cromwellTiming <- function(workflow_id) {
  if ("" %in% Sys.getenv("CROMWELLURL")) {
    stop("CROMWELLURL is not set.")
  }  else
    print("Getting timing diagram from Cromwell.")
  utils::browseURL(paste0(
    Sys.getenv("CROMWELLURL"),
    "/api/workflows/v1/",
    workflow_id,
    "/timing"
  ))
}