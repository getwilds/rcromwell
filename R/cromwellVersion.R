#' Get the version of a Cromwell server
#'
#' @export
#' @inheritSection workflow_options Important
#' @author Amy Paguirigan, Scott Chamberlain
#' @return (character) the Cromwell version
cromwell_version <- function() {
  check_url()
  http_get(make_url("engine/v1/version"))
}
