#' Validates a workflow for submission
#'
#'
#' @param wdl Local path to the wdl file describing the workflow. (Required)
#' @param all_inputs Local path to the json containing ALL the inputs the
#' parameters to use with the workflow - if a batch and parameters json
#' are present they must first be combined. (Optional)
#' @return Returns the response from the API post which includes the workflow
#' ID that you'll need to monitor the job.
#' @author Amy Paguirigan
#' @inheritSection workflowOptions Important
#' @export
cromwellValidate <- function(wdl, all_inputs = NULL) {
  check_url()
  crom_mssg("Validating a workflow for Cromwell")

  bodyList <- list(workflowSource = httr::upload_file(wdl))
  if (!is.null(all_inputs)) {
    bodyList <- c(
      bodyList,
      workflowInputs = list(httr::upload_file(all_inputs))
    )
  }

  httpPOST(
    url = make_url("api/womtool/v1/describe"),
    body = bodyList,
    encode = "multipart"
  )
}
