#' Submit a workflow job to Cromwell
#'
#' Supports the submission of a fully defined workflow job to a Cromwell instance.
#'
#' @param WDL Local path to the wdl file describing the workflow. (Required)
#' @param Params Local path to the json containing the parameters to use with the workflow. (Optional)
#' @param Batch Local path to the json containing a reference to any batch file desired if the workflow is a batch. (Optional)
#' @param Options Local path to the json containing workflow options to apply.(Optional)
#' @param Labels A data frame containing the labels for this workflow.(Optional)
#' @param Dependencies A zip'd file of subworkflow dependencies. (Optional)
#' @param cromURL The full string of the Cromwell URL to send jobs to (e.g. http://gizmog10:8000). (Optional)
#' @return Returns the response from the API post which includes the workflow ID that you'll need to monitor the job.
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell server URL to be set in the environment (CROMWELLURL), or the use
#' of the cromURL param if you want to specify upon call the URL to use.
#' @export
cromwellSubmitBatch <-
  function(WDL, Batch=NULL, Params=NULL, Options=NULL, Labels=NULL, Dependencies = NULL,
           cromURL = Sys.getenv("CROMWELLURL", unset = "needsURL")) {
    if(cromURL == "needsURL") {
      stop("CROMWELLURL is not set in your environment, or specify the URL to query via cromURL.")
    } else {
      message("Submitting a batch workflow to Cromwell.")
    }
    if(is.null(Batch) & is.null(Params) == T) {
      warning("You did not submit either Batch inputs or Params inputs for this workflow.  Was that on purpose?")
    }
    bodyList <- list(
      workflowSource = httr::upload_file(WDL))

    if(is.null(Params) == F & is.null(Batch) == F) { bodyList <- c(bodyList, workflowInputs = list(httr::upload_file(Params))); bodyList <- c(bodyList, workflowInputs_2 = list(httr::upload_file(Batch)))
    } else if (is.null(Params) == F) { bodyList <- c(bodyList, workflowInputs = list(httr::upload_file(Params)))
    } else if  (is.null(Batch) == F) { bodyList <- c(bodyList, workflowInputs = list(httr::upload_file(Batch))) }

    if(is.null(Dependencies) == F) bodyList <- c(bodyList, workflowDependencies = list(httr::upload_file(Dependencies)))
    if(is.null(Options) == F) bodyList <- c(bodyList, workflowOptions = list(httr::upload_file(Options)))
    if(is.null(Labels) == F) bodyList <- c(bodyList, labels = list(jsonlite::toJSON(as.list(Labels), auto_unbox = TRUE)))

    cromDat <-
      httpPOST(
        url = paste0(cromURL, "/api/workflows/v1"),
        body = bodyList,
        encode = "multipart"
      )
    cromResponse <-
      data.frame(cromDat, stringsAsFactors = F)
    return(cromResponse)
  }
