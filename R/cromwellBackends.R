#' Get the available backends of a Cromwell server
#'
#' @export
#' @author Amy Paguirigan, Scott Chamberlain
#' @inheritSection workflow_options Important
#' @return Cromwell backend options
cromwell_backends <- function() {
  check_url()
  crom_mssg("Getting backend options from Cromwell")
  all <- http_get(make_url("api/workflows/v1/backends"))
  all$supportedBackends <- unlist(all$supportedBackends)
  return(all)
}
