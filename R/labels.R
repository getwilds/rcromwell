#' Cromwell labels for a workflow
#'
#' @export
#' @template workflowid
#' @template serverdeets
#' @return a named list of workflow labels
cromwell_labels <- function(workflow_id, url = cw_url(), token = NULL) {
  check_url(url)
  response <- http_get(
    url = make_url(
      url,
      "api/workflows/v1",
      workflow_id,
      "labels"
    ),
    as = "parsed",
    token = token
  )
  labels <- response$labels
  names(labels)[grep("cromwell-workflow-id", names(labels))] <- "workflow_id"
  labels
}
