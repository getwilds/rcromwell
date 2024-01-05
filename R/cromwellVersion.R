#' Get the version of a Cromwell server
#'
#' @return (character) the Cromwell version
#' @author Amy Paguirigan
#' @inheritSection workflowOptions Important
#' @export
cromwellVersion <- function() {
  check_url()
  httpGET(make_url("engine/v1/version"))
}
