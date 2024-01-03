#' Pull Cromwell Call Caching Data
#'
#' Gets info about call caching status for the calls of a workflow
#'
#' @param workflow_id The workflow ID to return call caching metadata for.
#' @param cromURL The full string of the Cromwell URL to query (e.g. http://gizmog10:8000). (Optional)
#' @return Returns a long form data frame of metadata on call caching in a workflow. NOTE: Currently does not support subworkflows well.
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell server URL to be set in the environment, or the use
#' of the cromURL param if you want to specify upon call the URL to us.
#' @export
cromwellCache <- function(workflow_id, cromURL = Sys.getenv("CROMWELLURL", unset = "needsURL")) {
  if(cromURL == "needsURL") {
    stop("CROMWELLURL is not set in your environment, or specify the URL to query via cromURL.")
  } else {
    message(paste0("Querying for call caching metadata for workflow id: ", workflow_id)) }

    crommetadata <-
      httpGET(
        url = make_url("api/workflows/v1", workflow_id, "metadata"),
        query = list(expandSubWorkflows = "false"),
        as = "parsed")

  if (length(crommetadata$calls) > 0) {
    # if there are calls to be queried, continue
    bobCalls <-
      purrr::pluck(crommetadata, "calls") # we only want the calls data from the metadata for this workflow
    bobCallMeta <-
      purrr::map(bobCalls, function(callData) {
        # for each of the calls in the workflow...
        purrr::map_dfr(callData , function(shardData) {
          # and for each of the shards in that workflow...
          if ("inputs" %in% names(shardData) == T) {
            a <- purrr::keep(shardData,
                          names(shardData) %in% c("callCaching", "inputs", "outputs")) # select only these lists
            b <- data.frame(rbind(unlist(a)), stringsAsFactors = F) # flatten them and make them a data frame
            b$shardIndex <-
              shardData$shardIndex # add the shard Index associated
          } else {
            b <-
              data.frame("shardIndex" = shardData$shardIndex,
                         stringsAsFactors = F)
          }
          b$shardIndex <- as.character(b$shardIndex)
          b$workflow_id <- workflow_id
          b$executionStatus <- shardData$executionStatus
          b$returnCode <- shardData$returnCode
          b$jobId <- shardData$jobId
          b <- dplyr::select(b, -dplyr::starts_with("callCaching.hitFailures")) # then remove any data from the messy hitFailures lists
          return(b)
        })
      })
    geocache <- purrr::map_dfr(bobCallMeta, rbind, .id = "fullName")
    # split fullname into workflowName and callName
    geocache <- tidyr::separate(data = geocache,
                                 col = fullName,
                                 into = c("workflowName", "callName"),
                                 sep = "\\.",
                                 extra = "merge")
  } else {
    geocache <-
      data.frame("workflow_id" = "No call caching metadata available.", stringsAsFactors = F)
  }
  return(geocache)
}
