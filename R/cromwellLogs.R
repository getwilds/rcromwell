#' Gets logs for a workflow in Cromwell
#'
#' @export
#' @template workflowid
#' @template serverdeets
#' @author Amy Paguirigan, Scott Chamberlain
#' @inheritSection workflow_options Important
#' @return a tibble with a variable number of forws; with columns:
#' - callName (chr)
#' - stderr (chr)
#' - stdout (chr)
#' - attempt (chr)
#' - shardIndex (chr)
#' - workflow_id (chr)
cromwell_logs <- function(workflow_id, url = cw_url(), token = NULL) {
  check_url(url)
  crom_mssg("Getting list of logs from Cromwell")
  response <- cromwell_logs_query(workflow_id, url, token)
  cromwell_logs_process(response, workflow_id)
}

cromwell_logs_query <- function(workflow_id, url = cw_url(), token = NULL) {
  http_get(
    url = make_url(
      url,
      "api/workflows/v1",
      workflow_id,
      "logs"
    ),
    as = "parsed",
    token = token
  )
}

cromwell_logs_process <- function(response, workflow_id) {
  calls <- purrr::pluck(response, "calls")
  calls_flat <- purrr::map_dfr(calls, function(x) {
    purrr::map_dfr(x, function(s) {
      dplyr::as_tibble(rbind(unlist(s)))
    })
  }, .id = "callName")
  calls_flat$workflow_id <- workflow_id
  calls_flat
}
