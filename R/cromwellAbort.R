
#' Abort a workflow job on Cromwell
#'
#' Aborts any given workflow job on Cromwell.
#'
#' @param workflow_id Unique workflow id of the job you wish to kill.
#' @param cromURL The full string of the Cromwell URL to query (e.g. http://gizmog10:8000). (Optional)
#' @return Returns the response from the API post
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell server URL to be set in the environment, or the use
#' of the cromURL param if you want to specify upon call the URL to use.
#' @export
cromwellAbort <- function(workflow_id, cromURL = Sys.getenv("CROMWELLURL", unset = "needsURL")) {
  if(cromURL == "needsURL") {
    stop("CROMWELLURL is not set in your environment, or specify the URL to query via cromURL.")
  } else {
    print("Aborting job in Cromwell.") }
  cromAbort <-
    httpPOST(url =
      make_url("api/workflows/v1",
        workflow_id,
        "abort"
      )
    )
  cromResponse <-
    data.frame(cromAbort, stringsAsFactors = F)
  return(cromResponse)
}
