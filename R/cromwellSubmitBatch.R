#' Submit a workflow job to Cromwell
#'
#' Supports the submission of a fully defined workflow job to a Cromwell instance.
#'
#' @param WDL Local path to the wdl file describing the workflow. (Required)
#' @param Params Local path to the json containing the parameters to use with the workflow. (Optional)
#' @param Batch Local path to the json containing a reference to any batch file desired if the workflow is a batch. (Required)
#' @param Options Local path to the json containing workflow options to apply.(Optional)
#' @param Labels A data frame containing the labels for this workflow.(Optional)
#' @param Dependencies A zip'd file of subworkflow dependencies. (Optional)
#' @return Returns the response from the API post which includes the workflow ID that you'll need to monitor the job.
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell URL to be set in the environment.
#' @examples
#' TBD
#' @export
cromwellSubmitBatch <-
  function(WDL, Batch, Params=NULL, Options=NULL, Labels=NULL, Dependencies = NULL) {
    if("" %in% Sys.getenv("CROMWELLURL")) {
      stop("CROMWELLURL is not set.")
    } else
      print("Submitting a batch workflow to Cromwell.")
    if(is.null(Batch) & is.null(Params) == T) {
      stop("Either Batch inputs or Params inputs must be specified.")
    }
    bodyList <- list(
      workflowSource = httr::upload_file(WDL))

    if(is.null(Batch) == F) bodyList <- c(bodyList, workflowInputs = list(httr::upload_file(Batch)))
    if(is.null(Params) == F) bodyList <- c(bodyList, workflowInputs_2 = list(httr::upload_file(Params)))

    if(is.null(Dependencies) == F) bodyList <- c(bodyList, workflowDependencies = list(httr::upload_file(Dependencies)))
    if(is.null(Options) == F) bodyList <- c(bodyList, workflowOptions = list(httr::upload_file(Options)))
    if(is.null(Labels) == F) bodyList <- c(bodyList, labels = list(jsonlite::toJSON(as.list(Labels), auto_unbox = TRUE)))

    cromDat <-
      httr::POST(
        url = paste0(Sys.getenv("CROMWELLURL"), "/api/workflows/v1"),
        body = bodyList,
        encode = "multipart"
      )
    cromResponse <-
      data.frame(httr::content(cromDat), stringsAsFactors = F)
    return(cromResponse)
  }
