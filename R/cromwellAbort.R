#' Abort a workflow job on Cromwell
#'
#' @template workflowid
#' @return Returns the response from the API post
#' @author Amy Paguirigan
#' @inheritSection workflow_options Important
#' @export
cromwell_abort <- function(workflow_id) {
  check_url()
  crom_mssg("Aborting job in Cromwell")
  http_post(url = make_url("api/workflows/v1", workflow_id, "abort")) %>%
    dplyr::as_tibble()
}
