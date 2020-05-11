#' Pull a glob of metadata for a specific Cromwell workflow job
#'
#' Retrieve a glob of workflow level metadata for a specific workflow.
#'
#' @param workflow_id The workflow ID to return metadata for.
#' @param expandSubWorkflows Boolean, whether to expand subworkflows in the results or not, default is FALSE.
#' @return Returns a gross list of lists of metadata on a workflow.
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell server URL to be set in the environment. (use `setCromwellURL()`)
#' @examples
#' ## Request what jobs have been submitted to your Cromwell instance in the past 7 days.
#' recentJobs <- cromwellJobs(days = 7)
#' ## Request workflow metadata for a specific job that was run in your Cromwell instance.
#' thisWorkflowID <- recentJobs$workflow_id[1]
#' workflowMeta <- cromwellGlob(workflow_id = thisWorkflowID)
#' @export
cromwellGlob <- function(workflow_id, expandSubWorkflows = F) {
  if ("" %in% Sys.getenv("CROMWELLURL")) {
    stop("CROMWELLURL is not set.")
  } else {
    message(paste0("Querying for metadata for workflow id: ", workflow_id))
  }
  if (expandSubWorkflows == F) {
    crommetadata <-
      httr::content(httr::GET(
        paste0(
          Sys.getenv("CROMWELLURL"),
          "/api/workflows/v1/",
          workflow_id,
          "/metadata?expandSubWorkflows=false"
        )
      ), as = "parsed")
  }
  if (expandSubWorkflows == T) {
    crommetadata <-
      httr::content(httr::GET(
        paste0(
          Sys.getenv("CROMWELLURL"),
          "/api/workflows/v1/",
          workflow_id,
          "/metadata?expandSubWorkflows=true"
        )
      ), as = "parsed")
  }
  return(crommetadata)
}
