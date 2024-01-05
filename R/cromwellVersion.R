#' Get the version of a Cromwell server
#'
#' @return (character) the Cromwell version
#' @author Amy Paguirigan
#' @inheritSection workflow_options Important
#' @export
cromwell_version <- function() {
  check_url()
  http_get(make_url("engine/v1/version"))
}
