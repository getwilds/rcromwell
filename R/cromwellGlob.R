#' Pull a glob of metadata for a specific Cromwell workflow job
#'
#' Retrieve a glob of workflow level metadata for a specific workflow.
#'
#' @template workflowid
#' @param expandSubWorkflows Boolean, whether to expand subworkflows in the
#' results or not, default is FALSE.
#' @return Returns a gross list of lists of metadata on a workflow.
#' @author Amy Paguirigan
#' @inheritSection workflowOptions Important
#' @examples \dontrun{
#' ## Request what jobs have been submitted to your Cromwell instance in the
#' ## past 7 days.
#' recentJobs <- cromwellJobs(days = 7)
#' ## Request workflow metadata for a specific job that was run in your
#' ## Cromwell instance.
#' thisWorkflowID <- recentJobs$workflow_id[1]
#' workflowMeta <- cromwellGlob(workflow_id = thisWorkflowID)
#' }
#' @export
cromwellGlob <- function(workflow_id, expandSubWorkflows = FALSE) {
  check_url()
  crom_mssg(paste0("Querying for metadata for workflow id: ", workflow_id))
  url <- make_url("api/workflows/v1", workflow_id, "metadata")
  if (!expandSubWorkflows) {
    crommetadata <-
      httpGET(
        url = url,
        query = list(expandSubWorkflows = "false"),
        as = "parsed"
      )
  } else {
    crommetadata <-
      httpGET(
        url = url,
        query = list(expandSubWorkflows = "true"),
        as = "parsed"
      )
  }
  return(crommetadata)
}
