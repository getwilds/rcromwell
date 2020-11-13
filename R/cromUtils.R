#' Pull the workflow options provided for a Cromwell workflow job
#
#'
#' @param workflow_id The workflow ID to return options for.
#' @param cromURL The full string of the Cromwell URL to query if not using this locally (e.g. http://gizmog10:8000). (Optional)
#' @return Returns a data frame of the options for a workflow previously run
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell server URL to be set in the environment, or the use
#' of the cromURL param if you want to specify upon call the URL to use. (use `setCromwellURL()`)
#' @export
workflowOptions <- function(workflow_id, cromURL = Sys.getenv("CROMWELLURL", unset = "needsURL")) {
  if(cromURL == "needsURL") {
    stop("CROMWELLURL is not set in your environment, or specify the URL to query via cromURL.")
  } else {
    message(paste0("Querying for failure metadata for workflow id: ", workflow_id))
  }
  options <- as.data.frame(jsonlite::fromJSON(cromwellWorkflow(workflow_id)$options))
  return(options)
}
#' Pull the workflow inputs provided for a Cromwell workflow job
#
#'
#' @param workflow_id The workflow ID to return inputs for.
#' @param cromURL The full string of the Cromwell URL to query if not using this locally (e.g. http://gizmog10:8000). (Optional)
#' @return Returns a data frame of the inputs for a workflow previously run
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell server URL to be set in the environment, or the use
#' of the cromURL param if you want to specify upon call the URL to use. (use `setCromwellURL()`)
#' @export
workflowInputs <- function(workflow_id, cromURL = Sys.getenv("CROMWELLURL", unset = "needsURL")) {
  if(cromURL == "needsURL") {
    stop("CROMWELLURL is not set in your environment, or specify the URL to query via cromURL.")
  } else {
    message(paste0("Querying for failure metadata for workflow id: ", workflow_id))
  }
  inputs <- as.data.frame(jsonlite::fromJSON(cromwellWorkflow(workflow_id)$inputs))
  return(inputs)
}

