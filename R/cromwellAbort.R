#' Abort a workflow job on Cromwell
#'
#' @export
#' @template workflowid
#' @author Amy Paguirigan, Scott Chamberlain
#' @inheritSection workflow_options Important
#' @return Returns the response from the API post
cromwell_abort <- function(workflow_id) {
  check_url()
  crom_mssg("Aborting job in Cromwell")
  http_post(url = make_url("api/workflows/v1", workflow_id, "abort")) %>%
    dplyr::as_tibble()
}
