#' Gets outputs for a workflow in Cromwell
#'
#' @export
#' @template workflowid
#' @template serverdeets
#' @author Amy Paguirigan, Scott Chamberlain
#' @inheritSection workflow_options Important
#' @return a tibble with columns:
#' - workflowName
#' - workflowOutputType
#' - pathToOutput
#' - shardIndex
#' - workflow_id
cromwell_outputs <- function(workflow_id, url = cw_url(), token = NULL) {
  check_url(url)
  crom_mssg(glue("Querying for outputs list for workflow id: {workflow_id}"))
  resp <- cromwell_outputs_query(workflow_id, url, token)
  cromwell_outputs_process(resp, workflow_id)
}

cromwell_outputs_query <- function(workflow_id, url = cw_url(), token = NULL) {
  http_get(
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
}

#' @autoglobal
cromwell_outputs_process <- function(resp, workflow_id) {
  if (length(resp$outputs) == 0) return(dplyr::tibble())
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
  df
}
