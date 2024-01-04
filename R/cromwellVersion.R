#' Get the version of a Cromwell server
#'
#' @return (character) the Cromwell version
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell server URL to be set in the environment
#' @export
cromwellVersion <- function() {
  check_url()
  httpGET(make_url("engine/v1/version"))
}
