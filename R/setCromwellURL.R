#' Set the URL for your Cromwell server's API
#'
#'
#' @param nodeAndPort Your node and port of the running Cromwell server on Gizmo (e.g., gizmoz85:12345)
#' @return Sets your environment variable CROMWELLURL to be that of the Cromwell server for the job specified.
#' @author Amy Paguirigan
#' @export
setCromwellURL <- function(nodeAndPort = NULL) {
  if (is.null(nodeAndPort) == T) {
    stop("Please supply the node and port of your Cromwell server.")
  }
  Sys.setenv(CROMWELLURL=paste0("http://", nodeAndPort))
}
