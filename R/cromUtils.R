#' Pull the workflow options provided for a Cromwell workflow job
#'
#' @export
#' @template workflowid
#' @importFrom jsonlite fromJSON
#' @author Amy Paguirigan, Scott Chamberlain
#' @section Important:
#' Requires valid Cromwell server URL to be set. See [cromwell_settings]
#' @return Returns a data frame of the options for a workflow previously run
#' @examples \dontrun{
#' jobs <- cromwell_jobs()
#' workflow_options(jobs$workflow_id[1])
#' }
workflow_options <- function(workflow_id) {
  check_url()
  dplyr::as_tibble(jsonlite::fromJSON(cromwell_workflow(workflow_id)$options))
}

#' Pull the workflow inputs provided for a Cromwell workflow job
#'
#' @export
#' @template workflowid
#' @author Amy Paguirigan, Scott Chamberlain
#' @inheritSection workflow_options Important
#' @return Returns a data frame of the inputs for a workflow previously run
workflow_inputs <- function(workflow_id) {
  check_url()
  dplyr::as_tibble(jsonlite::fromJSON(cromwell_workflow(workflow_id)$inputs))
}
