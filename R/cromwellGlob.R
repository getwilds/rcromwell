#' Pull a glob of metadata for a specific Cromwell workflow job
#'
#' Retrieve a glob of workflow level metadata for a specific workflow.
#'
#' @export
#' @template workflowid
#' @template serverdeets
#' @param expand_sub_workflows Boolean, whether to expand subworkflows in the
#' results or not (default: `FALSE`)
#' @author Amy Paguirigan, Scott Chamberlain
#' @inheritSection workflow_options Important
#' @return a list of metadata on a workflow
#' @examples \dontrun{
#' ## Request what jobs have been submitted to your Cromwell instance in the
#' ## past 7 days.
#' recentJobs <- cromwell_jobs(days = 7)
#' ## Request workflow metadata for a specific job that was run in your
#' ## Cromwell instance.
#' thisWorkflowID <- recentJobs$workflow_id[1]
#' workflowMeta <- cromwell_glob(workflow_id = thisWorkflowID)
#' }
cromwell_glob <- function(
    workflow_id, expand_sub_workflows = FALSE,
    url = cw_url(), token = NULL) {
  stopifnot(rlang::is_logical(expand_sub_workflows))
  stopifnot(
    "expand_sub_workflows should be length 1" =
      length(expand_sub_workflows) == 1
  )
  check_url(url)
  crom_mssg(glue("Querying for metadata for workflow id: {workflow_id}"))
  url <- make_url(url, "api/workflows/v1", workflow_id, "metadata")
  http_req_get(url = url, token = token) |>
    req_url_query(expandSubWorkflows = tolower(expand_sub_workflows)) |>
    http_perform()
}
