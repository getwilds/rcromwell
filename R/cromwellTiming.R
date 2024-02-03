#' Get a timing diagram for a Cromwell workflow
#'
#' @export
#' @template workflowid
#' @template serverdeets
#' @inheritSection workflow_options Important
#' @author Amy Paguirigan, Scott Chamberlain
#' @return Opens a timing diagram in a browser
cromwell_timing <- function(workflow_id, url = cw_url()) {
  check_url(url)
  crom_mssg("Getting timing diagram from Cromwell")
  utils::browseURL(
    make_url(
      url,
      "api/workflows/v1",
      workflow_id,
      "timing"
    )
  )
}
