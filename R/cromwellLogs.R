
#' Gets logs for a workflow in Cromwell
#'
#'
#' @param workflow_id Unique workflow id of the job.
#' @return Returns the response from the API post
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell server URL to be set in the environment. (use `setCromwellURL()`)
#' @export
cromwellLogs <- function(workflow_id) {
  if ("" %in% Sys.getenv("CROMWELLURL")) {
    stop("CROMWELLURL is not set.")
  }  else
    message("Getting list of logs from Cromwell.")
  cromDat <-
    httr::GET(url = paste0(
      Sys.getenv("CROMWELLURL"),
      "/api/workflows/v1/",
      workflow_id,
      "/logs"
    ))
  cromResponse <- httr::content(cromDat, as = "parsed")
  calls <- purrr::pluck(cromResponse, "calls")
  callsFlat <- purrr::map_dfr(calls, function(x) {
    justcalls <- purrr::map_dfr(x, function(s) {
      shard <-
        data.frame(rbind(unlist(s)), stringsAsFactors = F) # flatten them and make them a data frame
    })
  }, .id = "callName")
  callsFlat$workflow_id <- workflow_id
  return(callsFlat)
}
