#' Gets outputs for a workflow in Cromwell
#'
#' @export
#' @template workflowid
#' @template serverdeets
#' @author Amy Paguirigan, Scott Chamberlain
#' @autoglobal
#' @inheritSection workflow_options Important
#' @return Returns a dataframe containing the workflowName, workflowOutputType,
#' pathToOutput, shardIndex for the specified outputs of a workflow_id
cromwell_outputs <- function(workflow_id, url = cw_url(), token = NULL) {
  check_url(url)
  crom_mssg(paste0("Querying for outputs list for workflow id: ", workflow_id))
  # Make API call for output content and parse the content returned
  resp <- http_get(
    url =
      make_url(
        url,
        "api/workflows/v1",
        workflow_id,
        "outputs"
      ),
    as = "parsed",
    token = token
  )
  # if the length of the outputs is 0 then there are no outputs available yet.
  if (length(resp$outputs) > 0) {
    # grab only the outputs list and unlist into a dataframe
    df <- purrr::map_dfr(resp$outputs, function(x) {
      z <- dplyr::tibble("pathToOutput" = unlist(x))
      z$shardIndex <- gsub("/.*$", "", gsub("^.*shard-", "", z$pathToOutput))
      z
    }, .id = "workflowMeta")
    df$shardIndex[df$shardIndex == ""] <- NA
    # separate out the workflowName and workflowOutputType columns
    df <- tidyr::separate(df, workflowMeta,
      c("workflowName", "workflowOutputType"),
      sep = "\\."
    )
    # if the outputs are in S3, then create the prefix and the bucket as
    # separate columns
    if (sum(grepl("^s3://", df$pathToOutput)) > 0) {
      df$s3Prefix <- gsub("s3://[^/]*/", "", df$pathToOutput)
      df$s3Bucket <-
        gsub("/.*$", "", gsub("s3://", "", df$pathToOutput))
    }
    df$workflow_id <- workflow_id
  } else {
    df <- dplyr::tibble(
      "workflow_id" =
        "No outputs available for this workflow yet"
    )
  }
  return(df)
}
