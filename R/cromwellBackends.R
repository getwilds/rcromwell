#' Get the available backends of a Cromwell server
#'
#' @export
#' @template serverdeets
#' @author Amy Paguirigan, Scott Chamberlain
#' @inheritSection workflow_options Important
#' @return Cromwell backend options as a list
cromwell_backends <- function(url = cw_url(), token = NULL) {
  check_url(url)
  crom_mssg("Getting backend options from Cromwell")
  all <- http_req_get(
    url = make_url(url, "api/workflows/v1/backends"),
    token = token
  ) |>
    http_perform()
  all$supportedBackends <- unlist(all$supportedBackends)
  return(all)
}
