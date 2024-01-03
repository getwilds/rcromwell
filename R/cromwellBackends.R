#' Get the available backends of a Cromwell server
#'
#'
#' @return Cromwell backend options
#' @param cromURL The full string of the Cromwell URL to query (e.g. http://gizmog10:8000). (Optional)
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell server URL to be set in the environment, or the use
#' of the cromURL param if you want to specify upon call the URL to use.
#' @export
cromwellBackends <- function(cromURL = Sys.getenv("CROMWELLURL", unset = "needsURL")) {
  if(cromURL == "needsURL") {
    stop("CROMWELLURL is not set in your environment, or specify the URL to query via cromURL.")
  } else {
    print("Getting backend options from Cromwell.")
  }
  all <- httpGET(make_url("api/workflows/v1/backends"))
  all$supportedBackends <- unlist(all$supportedBackends)
  return(all)
}
