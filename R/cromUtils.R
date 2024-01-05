#' Pull the workflow options provided for a Cromwell workflow job
#'
#' @template workflowid
#' @importFrom jsonlite fromJSON
#' @return Returns a data frame of the options for a workflow previously run
#' @author Amy Paguirigan
#' @section Important:
#' Requires valid Cromwell server URL to be set. See [cromwellSettings]
#' @export
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
#' @template workflowid
#' @return Returns a data frame of the inputs for a workflow previously run
#' @author Amy Paguirigan
#' @inheritSection workflow_options Important
#' @export
workflow_inputs <- function(workflow_id) {
  check_url()
  dplyr::as_tibble(jsonlite::fromJSON(cromwell_workflow(workflow_id)$inputs))
}
