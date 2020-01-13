
#' Abort a workflow job on Cromwell
#'
#' Aborts any given workflow job on Cromwell.
#'
#' @param workflow_id Unique workflow id of the job you wish to kill.
#' @return Returns the response from the API post
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell URL to be set in the environment.
#' @export
cromwellAbort <- function(workflow_id) {
  if ("" %in% Sys.getenv("CROMWELLURL")) {
    stop("CROMWELLURL is not set.")
  } else
    print("Aborting job in Cromwell.")
  cromAbort <-
    httr::POST(url = paste0(
      Sys.getenv("CROMWELLURL"),
      "/api/workflows/v1/",
      workflow_id,
      "/abort"
    ))
  cromResponse <-
    data.frame(httr::content(cromAbort), stringsAsFactors = F)
  return(cromResponse)
}
