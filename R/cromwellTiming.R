#' Get a timing diagram for a Cromwell workflow
#'
#'
#' @param workflow_id Unique workflow id of the job.
#'@param cromURL The full string of the Cromwell URL to query (e.g. http://gizmog10:8000). (Optional)
#' @return Opens a timing diagram in a browser
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell server URL to be set in the environment, or the use
#' of the cromURL param if you want to specify upon call the URL to use.
#' @export
cromwellTiming <- function(workflow_id, cromURL = Sys.getenv("CROMWELLURL", unset = "needsURL")) {
  if(cromURL == "needsURL") {
    stop("CROMWELLURL is not set in your environment, or specify the URL to query via cromURL.")
  } else {
    print("Getting timing diagram from Cromwell.") }
  utils::browseURL(
    make_url(
      "api/workflows/v1",
      workflow_id,
      "timing"
    )
  )
}
