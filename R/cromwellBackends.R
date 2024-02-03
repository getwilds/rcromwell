#' Get the available backends of a Cromwell server
#'
#' @export
#' @template serverdeets
#' @author Amy Paguirigan, Scott Chamberlain
#' @inheritSection workflow_options Important
#' @return Cromwell backend options
cromwell_backends <- function(url = cw_url(), token = NULL) {
  check_url(url)
  crom_mssg("Getting backend options from Cromwell")
  all <- http_get(make_url(url, "api/workflows/v1/backends"), token = token)
  all$supportedBackends <- unlist(all$supportedBackends)
  return(all)
}
