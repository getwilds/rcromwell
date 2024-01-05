#' Abort a workflow job on Cromwell
#'
#' @template workflowid
#' @return Returns the response from the API post
#' @author Amy Paguirigan
#' @inheritSection workflowOptions Important
#' @export
cromwellAbort <- function(workflow_id) {
  check_url()
  crom_mssg("Aborting job in Cromwell")
  httpPOST(url = make_url("api/workflows/v1", workflow_id, "abort")) %>%
    dplyr::as_tibble()
}
