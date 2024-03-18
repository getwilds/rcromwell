#' Pull Cromwell Call Caching Data
#'
#' Gets info about call caching status for the calls of a workflow
#'
#' @export
#' @template workflowid
#' @template serverdeets
#' @author Amy Paguirigan, Scott Chamberlain
#' @inheritSection workflow_options Important
#' @details Currently does not support subworkflows well.
#' @return a tibble of metadata on call caching in a workflow; columns
#' vary depending on workflow; if no results, a zero row tibble
cromwell_cache <- function(workflow_id, url = cw_url(), token = NULL) {
  check_url(url)
  crom_mssg(glue(
    "Querying for call caching metadata for workflow id: {workflow_id}"
  ))
  crommetadata <- cromwell_cache_http(workflow_id, url, token)
  cromwell_cache_process(crommetadata, workflow_id)
}

cromwell_cache_http <- function(workflow_id, url = cw_url(), token = NULL) {
  http_get(
    url = make_url(url, "api/workflows/v1", workflow_id, "metadata"),
    query = list(expandSubWorkflows = "false"),
    as = "parsed",
    token = token
  )
}

#' @autoglobal
cromwell_cache_process <- function(crommetadata, workflow_id) {
  if (length(crommetadata$calls) == 0) {
    return(dplyr::tibble())
  }
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
        dplyr::select(b, -dplyr::starts_with("callCaching.hitFailures"))
      })
    })
  geocache <- purrr::map_dfr(bob_call_meta, rbind, .id = "fullName")
  # split fullname into workflowName and callName
  tidyr::separate(
    data = geocache,
    col = fullName,
    into = c("workflowName", "callName"),
    sep = "\\.",
    extra = "merge"
  )
}
