#' Get the available backends of a Cromwell server
#'
#' @return Cromwell backend options
#' @author Amy Paguirigan
#' @inheritSection workflow_options Important
#' @export
cromwell_backends <- function() {
  check_url()
  crom_mssg("Getting backend options from Cromwell")
  all <- http_get(make_url("api/workflows/v1/backends"))
  all$supportedBackends <- unlist(all$supportedBackends)
  return(all)
}
