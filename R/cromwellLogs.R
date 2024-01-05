#' Gets logs for a workflow in Cromwell
#'
#' @template workflowid
#' @return Returns the response from the API post
#' @author Amy Paguirigan
#' @inheritSection workflow_options Important
cromwell_logs <- function(workflow_id) {
  check_url()
  crom_mssg("Getting list of logs from Cromwell")
  response <-
    http_get(
      url = make_url(
        "api/workflows/v1",
        workflow_id,
        "logs"
      ),
      as = "parsed"
    )
  calls <- purrr::pluck(response, "calls")
  calls_flat <- purrr::map_dfr(calls, function(x) {
    purrr::map_dfr(x, function(s) {
      # flatten them and make them a data frame
      dplyr::as_tibble(rbind(unlist(s)))
    })
  }, .id = "callName")
  calls_flat$workflow_id <- workflow_id
  return(calls_flat)
}
