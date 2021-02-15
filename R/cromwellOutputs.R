
#' Gets outputs for a workflow in Cromwell
#'
#'
#'
#' @param workflow_id Unique workflow id of the job.
#' @param cromURL The full string of the Cromwell URL to query if not using this locally (e.g. http://gizmog10:8000). (Optional)
#' @return Returns a dataframe containing the workflowName, workflowOutputType,
#' pathToOutput, shardIndex for the specified outputs of a workflow_id
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell server URL to be set in the environment, or the use
#' of the cromURL param if you want to specify upon call the URL to use. (use `setCromwellURL()`)
#' @export
cromwellOutputs <- function(workflow_id, cromURL = Sys.getenv("CROMWELLURL", unset = "needsURL")) {
  if(cromURL == "needsURL") {
    stop("CROMWELLURL is not set in your environment, or specify the URL to query via cromURL.")
  } else {
    message(paste0("Querying for outputs list for workflow id: ", workflow_id))
  }
  # Make API call for output content and parse the content returned
  cromResponse <-httr::content(
    httr::GET(url = paste0(
      cromURL,
      "/api/workflows/v1/",
      workflow_id,
      "/outputs"
    )), as  = "parsed")
  # if the length of the outputs is 0 then there are no outputs available yet.
  if (length(cromResponse$outputs) > 0) {
    # grab only the outputs list and unlist into a dataframe
    outputsDf <- purrr::map_dfr(cromResponse$outputs, function(x) {
      Z <- data.frame("pathToOutput" = unlist(x), stringsAsFactors = F)
      Z$shardIndex <- gsub("/.*$", "", gsub("^.*shard-", "", Z$pathToOutput))
      Z
    }, .id = "workflowMeta")
    outputsDf$shardIndex[outputsDf$shardIndex == ""] <- NA
    # separate out the workflowName and workflowOutputType columns
    outputsDf <- tidyr::separate(outputsDf, workflowMeta,
                                 c("workflowName","workflowOutputType"),
                                 sep = "\\.")
    # if the outputs are in S3, then create the prefix and the bucket as separate columns
    if(sum(grepl("^s3://", outputsDf$pathToOutput)) > 0){
      outputsDf$s3Prefix <- gsub("s3://[^/]*/", "", outputsDf$pathToOutput)
      outputsDf$s3Bucket <-
        gsub("/.*$", "", gsub("s3://", "", outputsDf$pathToOutput))
    }
    outputsDf$workflow_id <- workflow_id
  } else {
    outputsDf <-
      data.frame("workflow_id" = "No outputs are available for this workflow yet.",  stringsAsFactors = F)
  }
  return(outputsDf)
}
