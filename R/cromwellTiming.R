#' Get a timing diagram for a Cromwell workflow
#'
#' @template workflowid
#' @return Opens a timing diagram in a browser
#' @author Amy Paguirigan
#' @inheritSection workflow_options Important
#' @export
cromwell_timing <- function(workflow_id) {
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
