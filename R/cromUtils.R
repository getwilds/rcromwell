#' Pull the workflow options provided for a Cromwell workflow job
#'
#' @param workflow_id The workflow ID to return options for.
#' @return Returns a data frame of the options for a workflow previously run
#' @author Amy Paguirigan
#' @section Important:
#' Requires valid Cromwell server URL to be set. See [cromwellSettings]
#' @export
workflowOptions <- function(workflow_id) {
  check_url()
  options <- as.data.frame(jsonlite::fromJSON(cromwellWorkflow(workflow_id)$options))
  return(options)
}

#' Pull the workflow inputs provided for a Cromwell workflow job
#'
#' @param workflow_id The workflow ID to return inputs for.
#' @return Returns a data frame of the inputs for a workflow previously run
#' @author Amy Paguirigan
#' @inheritSection workflowOptions Important
#' @export
workflowInputs <- function(workflow_id) {
  check_url()
  inputs <- as.data.frame(jsonlite::fromJSON(cromwellWorkflow(workflow_id)$inputs))
  return(inputs)
}
