#' Gets outputs for a workflow in Cromwell
#'
#' @template workflowid
#' @return Returns a dataframe containing the workflowName, workflowOutputType,
#' pathToOutput, shardIndex for the specified outputs of a workflow_id
#' @author Amy Paguirigan
#' @autoglobal
#' @inheritSection workflowOptions Important
#' @export
cromwellOutputs <- function(workflow_id) {
  check_url()
  crom_mssg(paste0("Querying for outputs list for workflow id: ", workflow_id))
  # Make API call for output content and parse the content returned
  cromResponse <- httpGET(
    url =
      make_url(
        "api/workflows/v1",
        workflow_id,
        "outputs"
      ), as = "parsed"
  )
  # if the length of the outputs is 0 then there are no outputs available yet.
  if (length(cromResponse$outputs) > 0) {
    # grab only the outputs list and unlist into a dataframe
    outputsDf <- purrr::map_dfr(cromResponse$outputs, function(x) {
      Z <- dplyr::tibble("pathToOutput" = unlist(x))
      Z$shardIndex <- gsub("/.*$", "", gsub("^.*shard-", "", Z$pathToOutput))
      Z
    }, .id = "workflowMeta")
    outputsDf$shardIndex[outputsDf$shardIndex == ""] <- NA
    # separate out the workflowName and workflowOutputType columns
    outputsDf <- tidyr::separate(outputsDf, workflowMeta,
      c("workflowName", "workflowOutputType"),
      sep = "\\."
    )
    # if the outputs are in S3, then create the prefix and the bucket as
    # separate columns
    if (sum(grepl("^s3://", outputsDf$pathToOutput)) > 0) {
      outputsDf$s3Prefix <- gsub("s3://[^/]*/", "", outputsDf$pathToOutput)
      outputsDf$s3Bucket <-
        gsub("/.*$", "", gsub("s3://", "", outputsDf$pathToOutput))
    }
    outputsDf$workflow_id <- workflow_id
  } else {
    outputsDf <-
      dplyr::tibble("workflow_id" = "No outputs available for this workflow yet")
  }
  return(outputsDf)
}
