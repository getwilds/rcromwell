#' Validates a workflow for submission
#'
#'
#' @param WDL Local path to the wdl file describing the workflow. (Required)
#' @param allInputs Local path to the json containing ALL the inputs the parameters to use with the workflow - if a batch and parameters json are present they must first be combined. (Optional)
#' @param cromURL The full string of the Cromwell URL to query (e.g. http://gizmog10:8000). (Optional)
#' @return Returns the response from the API post which includes the workflow ID that you'll need to monitor the job.
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell server URL to be set in the environment, or the use
#' of the cromURL param if you want to specify upon call the URL to use.
#' @examples
#' TBD
#' @export
womtoolValidate <-
  function(WDL, allInputs=NULL, cromURL = Sys.getenv("CROMWELLURL", unset = "needsURL")) {
    if(cromURL == "needsURL") {
      stop("CROMWELLURL is not set in your environment, or specify the URL to query via cromURL.")
    } else {
      message("Validating a workflow for Cromwell.") }

    bodyList <- list(workflowSource = httr::upload_file(WDL))
    if(is.null(allInputs) == F) bodyList <- c(bodyList, workflowInputs = list(httr::upload_file(allInputs)))

    cromDat <-
      httr::POST(
        url = paste0(cromURL, "/api/womtool/v1/describe"),
        body = bodyList,
        encode = "multipart"
      )
    cromResponse <-httr::content(cromDat)
    return(cromResponse)
  }
