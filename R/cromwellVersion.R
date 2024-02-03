#' Get the version of a Cromwell server
#'
#' @export
#' @template serverdeets
#' @inheritSection workflow_options Important
#' @author Amy Paguirigan, Scott Chamberlain
#' @return (character) the Cromwell version
cromwell_version <- function(url = cw_url(), token = NULL) {
  check_url(url)
  http_get(make_url(url, "engine/v1/version"), token = token)
}
