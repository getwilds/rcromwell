#' Abort a workflow job on Cromwell
#'
#' @export
#' @template workflowid
#' @template serverdeets
#' @author Amy Paguirigan, Scott Chamberlain
#' @inheritSection workflow_options Important
#' @return a tibble
cromwell_abort <- function(workflow_id, url = cw_url(), token = NULL) {
  check_url(url)
  crom_mssg("Aborting job in Cromwell")
  http_post(make_url(url, "api/workflows/v1", workflow_id, "abort"),
    token = token
  ) %>%
    dplyr::as_tibble()
}
