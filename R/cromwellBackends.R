#' Get the available backends of a Cromwell server
#'
#' @return Cromwell backend options
#' @author Amy Paguirigan
#' @inheritSection workflowOptions Important
#' @export
cromwellBackends <- function() {
  check_url()
  crom_mssg("Getting backend options from Cromwell")
  all <- httpGET(make_url("api/workflows/v1/backends"))
  all$supportedBackends <- unlist(all$supportedBackends)
  return(all)
}
