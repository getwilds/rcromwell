
#' Get the version of a Cromwell server
#'
#'@param cromURL The full string of the Cromwell URL to query  (e.g. http://gizmog10:8000). (Optional)
#' @return Cromwell version
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell server URL to be set in the environment or specify "cromURL".
#' @examples
#' TBD
#' @export
cromwellVersion <- function(cromURL = Sys.getenv("CROMWELLURL", unset = "needsURL")) {
  if(cromURL == "needsURL") {
    stop("CROMWELLURL is not set in your environment, or specify the URL to query via cromURL.")
  } else {
    message("Getting timing diagram from Cromwell.")
  }
  httpGET(make_url("engine/v1/version"))
}
