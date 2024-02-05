#' Get a timing diagram for a Cromwell workflow
#'
#' @export
#' @template workflowid
#' @param url (character) base url for your Cromwell server. optional. if not
#' supplied set the url as the env var `CROMWELLURL`
#' @inheritSection workflow_options Important
#' @author Amy Paguirigan, Scott Chamberlain
#' @details Internally this function uses [httr::BROWSE()] which uses
#' [utils::browseURL()] - if a auth header is required you'll have to
#' do that manually
#' @return Opens a timing diagram in a browser
cromwell_timing <- function(workflow_id, url = cw_url()) {
  check_url(url)
  crom_mssg("Getting timing diagram from Cromwell")
  httr::BROWSE(
    make_url(
      url,
      "api/workflows/v1",
      workflow_id,
      "timing"
    )
  )
}
