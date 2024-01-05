#' Pull the workflow options provided for a Cromwell workflow job
#'
#' @template workflowid
#' @return Returns a data frame of the options for a workflow previously run
#' @author Amy Paguirigan
#' @section Important:
#' Requires valid Cromwell server URL to be set. See [cromwellSettings]
#' @export
#' @examples \dontrun{
#' jobs <- cromwellJobs()
#' workflowOptions(jobs$workflow_id[1])
#' }
workflowOptions <- function(workflow_id) {
  check_url()
  dplyr::as_tibble(jsonlite::fromJSON(cromwellWorkflow(workflow_id)$options))
}

#' Pull the workflow inputs provided for a Cromwell workflow job
#'
#' @template workflowid
#' @return Returns a data frame of the inputs for a workflow previously run
#' @author Amy Paguirigan
#' @inheritSection workflowOptions Important
#' @export
workflowInputs <- function(workflow_id) {
  check_url()
  dplyr::as_tibble(jsonlite::fromJSON(cromwellWorkflow(workflow_id)$inputs))
}
