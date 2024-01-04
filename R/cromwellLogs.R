
#' Gets logs for a workflow in Cromwell
#'
#'
#' @param workflow_id Unique workflow id of the job.
#' @return Returns the response from the API post
#' @author Amy Paguirigan
#' @details Requires valid Cromwell server URL to be set in the environment
cromwellLogs <- function(workflow_id) {
  check_url()
  crom_mssg("Getting list of logs from Cromwell")
  cromResponse <-
    httpGET(url = make_url(
      "api/workflows/v1",
      workflow_id,
      "logs"
    ),
    as = "parsed"
  )
  calls <- purrr::pluck(cromResponse, "calls")
  callsFlat <- purrr::map_dfr(calls, function(x) {
    justcalls <- purrr::map_dfr(x, function(s) {
      # flatten them and make them a data frame
      shard <-
        data.frame(rbind(unlist(s)), stringsAsFactors = F)
    })
  }, .id = "callName")
  callsFlat$workflow_id <- workflow_id
  return(callsFlat)
}
