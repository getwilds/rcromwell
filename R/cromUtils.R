#' Pull the workflow options provided for a Cromwell workflow job
#'
#' @export
#' @template workflowid
#' @template serverdeets
#' @importFrom jsonlite fromJSON
#' @author Amy Paguirigan, Scott Chamberlain
#' @section Important:
#' Requires valid Cromwell server URL to be set. See [cromwell_settings]
#' @return a tibble of the options for a workflow previously run:
#' - read_from_cache
#' - workflow_failure_mode
#' - write_to_cache
#' @examples \dontrun{
#' jobs <- cromwell_jobs()
#' workflow_options(jobs$workflow_id[1])
#' }
workflow_options <- function(workflow_id, url = cw_url(), token = NULL) {
  check_url(url)
  dplyr::as_tibble(
    jsonlite::fromJSON(cromwell_workflow(workflow_id, url, token)$options)
  )
}

#' Pull the workflow inputs provided for a Cromwell workflow job
#'
#' @export
#' @template workflowid
#' @template serverdeets
#' @author Amy Paguirigan, Scott Chamberlain
#' @inheritSection workflow_options Important
#' @return a tibble of the inputs for a workflow previously run
workflow_inputs <- function(workflow_id, url = cw_url(), token = NULL) {
  check_url(url)
  dplyr::as_tibble(
    jsonlite::fromJSON(cromwell_workflow(workflow_id, url, token)$inputs)
  )
}
