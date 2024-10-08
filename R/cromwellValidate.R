#' Validates a workflow for submission
#'
#' @export
#' @param wdl Local path to the wdl file describing the workflow. (Required)
#' @param all_inputs Local path to the json containing ALL the inputs the
#' parameters to use with the workflow - if a batch and parameters json
#' are present they must first be combined. (Optional)
#' @author Amy Paguirigan, Scott Chamberlain
#' @inheritSection workflow_options Important
#' @template serverdeets
#' @return a list with validation details
cromwell_validate <- function(
    wdl, all_inputs = NULL,
    url = cw_url(), token = NULL) {
  check_url(url)
  crom_mssg("Validating a workflow for Cromwell")

  body <- list(workflowSource = curl::form_file(wdl))
  if (!is.null(all_inputs)) {
    body <- c(
      body,
      workflowInputs = list(curl::form_file(all_inputs))
    )
  }

  http_req_post(
    url = make_url(url, "api/womtool/v1/describe"),
    token = token
  ) |>
    req_body_multipart(!!!body) |>
    http_perform()
}
