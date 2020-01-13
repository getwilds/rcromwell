
#' Gets outputs for a workflow in Cromwell
#'
#'
#'
#' @param workflow_id Unique workflow id of the job.
#' @return Returns the response from the API post
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell URL to be set in the environment.
#' @export
cromwellOutputs <- function(workflow_id) {
  if ("" %in% Sys.getenv("CROMWELLURL")) {
    stop("CROMWELLURL is not set.")
  } else {
    print(paste0("Querying for outputs list for workflow id: ", workflow_id))
  }
  cromOut <-
    httr::GET(url = paste0(
      Sys.getenv("CROMWELLURL"),
      "/api/workflows/v1/",
      workflow_id,
      "/outputs"
    ))
  cromResponse <- httr::content(cromOut, as = "parsed")
  if (length(cromResponse$outputs) > 0) {
    outputsDf <- purrr::map_dfr(cromResponse$outputs, function(x) {
      Z <- data.frame("s3URL" = unlist(x), stringsAsFactors = F)
      dplyr::mutate(Z, shardIndex = gsub("/.*$", "", gsub("^.*shard-", "", Z$s3URL)))
    }, .id = "workflowOutputType")
    outputsDf$s3Prefix <- gsub("s3://[^/]*/", "", outputsDf$s3URL)
    outputsDf$s3Bucket <-
      gsub("/.*$", "", gsub("s3://", "", outputsDf$s3URL))
    outputsDf$workflow_id <- workflow_id
    outputsDf$workflowName <-
      gsub("/.*$",
           "",
           gsub("cromwell-output/", "", outputsDf$s3Prefix))
  } else {
    print("No outputs are available for this workflow.")
  }
  return(outputsDf)
}
