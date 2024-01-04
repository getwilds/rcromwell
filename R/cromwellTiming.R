#' Get a timing diagram for a Cromwell workflow
#'
#'
#' @param workflow_id Unique workflow id of the job.
#' @return Opens a timing diagram in a browser
#' @author Amy Paguirigan
#' @inheritSection workflowOptions Important
#' @export
cromwellTiming <- function(workflow_id) {
  check_url()
  crom_mssg("Getting timing diagram from Cromwell")
  utils::browseURL(
    make_url(
      "api/workflows/v1",
      workflow_id,
      "timing"
    )
  )
}
