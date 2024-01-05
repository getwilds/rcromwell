#' Validates a workflow for submission
#'
#'
#' @param WDL Local path to the wdl file describing the workflow. (Required)
#' @param allInputs Local path to the json containing ALL the inputs the
#' parameters to use with the workflow - if a batch and parameters json
#' are present they must first be combined. (Optional)
#' @return Returns the response from the API post which includes the workflow
#' ID that you'll need to monitor the job.
#' @author Amy Paguirigan
#' @inheritSection workflowOptions Important
#' @export
cromwellValidate <- function(WDL, allInputs = NULL) {
  check_url()
  crom_mssg("Validating a workflow for Cromwell")

  bodyList <- list(workflowSource = httr::upload_file(WDL))
  if (!is.null(allInputs)) {
    bodyList <- c(
      bodyList,
      workflowInputs = list(httr::upload_file(allInputs))
    )
  }

  httpPOST(
    url = make_url("api/womtool/v1/describe"),
    body = bodyList,
    encode = "multipart"
  )
}
