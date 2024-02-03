#' Gets logs for a workflow in Cromwell
#'
#' @export
#' @template workflowid
#' @template serverdeets
#' @author Amy Paguirigan, Scott Chamberlain
#' @inheritSection workflow_options Important
#' @return Returns the response from the API post
cromwell_logs <- function(workflow_id, url = cw_url(), token = NULL) {
  check_url(url)
  crom_mssg("Getting list of logs from Cromwell")
  response <-
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
