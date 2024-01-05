#' Pull Cromwell Call Caching Data
#'
#' Gets info about call caching status for the calls of a workflow
#'
#' @template workflowid
#' @return Returns a long form data frame of metadata on call caching in a
#' workflow. NOTE: Currently does not support subworkflows well.
#' @author Amy Paguirigan
#' @autoglobal
#' @inheritSection workflow_options Important
#' @export
cromwell_cache <- function(workflow_id) {
  check_url()
  crom_mssg(paste0(
    "Querying for call caching metadata for workflow id: ",
    workflow_id
  ))

  crommetadata <-
    http_get(
      url = make_url("api/workflows/v1", workflow_id, "metadata"),
      query = list(expandSubWorkflows = "false"),
      as = "parsed"
    )

  if (length(crommetadata$calls) > 0) {
    # if there are calls to be queried, continue
    # we only want the calls data from the metadata for this workflow
    bob_calls <- purrr::pluck(crommetadata, "calls")
    bob_call_meta <-
      purrr::map(bob_calls, function(call_data) {
        # for each of the calls in the workflow...
        purrr::map_dfr(call_data, function(shard_data) {
          # and for each of the shards in that workflow...
          if ("inputs" %in% names(shard_data)) {
            a <- purrr::keep(
              shard_data,
              names(shard_data) %in% c("callCaching", "inputs", "outputs")
            ) # select only these lists
            # flatten them and make them a data frame
            b <- dplyr::as_tibble(rbind(unlist(a)))
            # add the shard Index associated
            b$shardIndex <- shard_data$shardIndex
          } else {
            b <- dplyr::as_tibble("shardIndex" = shard_data$shardIndex)
          }
          b$shardIndex <- as.character(b$shardIndex)
          b$workflow_id <- workflow_id
          b$executionStatus <- shard_data$executionStatus
          b$returnCode <- shard_data$returnCode
          b$jobId <- shard_data$jobId
          # then remove any data from the messy hitFailures lists
          b <- dplyr::select(b, -dplyr::starts_with("callCaching.hitFailures"))
          return(b)
        })
      })
    geocache <- purrr::map_dfr(bob_call_meta, rbind, .id = "fullName")
    # split fullname into workflowName and callName
    geocache <- tidyr::separate(
      data = geocache,
      col = fullName,
      into = c("workflowName", "callName"),
      sep = "\\.",
      extra = "merge"
    )
  } else {
    geocache <-
      dplyr::as_tibble("workflow_id" = "No call caching metadata available.")
  }
  return(geocache)
}
