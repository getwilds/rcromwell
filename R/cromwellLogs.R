#' Gets logs for a workflow in Cromwell
#'
#' @template workflowid
#' @return Returns the response from the API post
#' @author Amy Paguirigan
#' @inheritSection workflowOptions Important
cromwellLogs <- function(workflow_id) {
  check_url()
  crom_mssg("Getting list of logs from Cromwell")
  cromResponse <-
    httpGET(
      url = make_url(
        "api/workflows/v1",
        workflow_id,
        "logs"
      ),
      as = "parsed"
    )
  calls <- purrr::pluck(cromResponse, "calls")
  callsFlat <- purrr::map_dfr(calls, function(x) {
    purrr::map_dfr(x, function(s) {
      # flatten them and make them a data frame
      dplyr::as_tibble(rbind(unlist(s)))
    })
  }, .id = "callName")
  callsFlat$workflow_id <- workflow_id
  return(callsFlat)
}
