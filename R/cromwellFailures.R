#' Pull metadata for the failed calls made in a Cromwell workflow job
#'
#' Gets info about failed calls for a specific workflow
#'
#' @export
#' @template workflowid
#' @template serverdeets
#' @author Amy Paguirigan, Scott Chamberlain
#' @inheritSection workflow_options Important
#' @return a tibble of metadata on failed calls in a workflow; columns
#' vary depending on workflow; if no results, a zero row tibble
#' @examples \dontrun{
#' ## Request what jobs have been submitted to your Cromwell instance in the
#' ## past 7 days.
#' recentJobs <- cromwell_jobs(days = 7)
#' ## Request workflow metadata for a specific job that was run in your
#' ## Cromwell instance.
#' thisWorkflowID <- recentJobs$workflow_id[1]
#' failsMeta <- cromwell_failures(workflow_id = thisWorkflowID)
#' }
cromwell_failures <- function(workflow_id, url = cw_url(), token = NULL) {
  check_url(url)
  crom_mssg(glue(
    "Querying for failure metadata for workflow id: {workflow_id}"
  ))

  response <-
    http_req_get(
      url = make_url(url, "api/workflows/v1", workflow_id, "metadata"),
      token = token
    ) |>
    req_url_query(includeKey = "failures", includeKey = "jobId") |>
    http_perform()
  cromwell_failures_process(response, workflow_id)
}

#' @autoglobal
cromwell_failures_process <- function(response, workflow_id) {
  if (!is.list(response$calls)) {
    return(dplyr::tibble())
  }
  bobfail <- purrr::pluck(response, "calls")
  if (sum(grepl("ScatterAt", names(bobfail))) > 0) {
    subs <- names(bobfail)[grepl("ScatterAt", names(bobfail))]
    sub_workflow <- bobfail[subs]
    bobfail <- bobfail[!names(bobfail) %in% subs]
    subworkflow_meta <- purrr::map(subs, function(x) {
      b <- purrr::flatten(sub_workflow[x])
      names(b) <- paste0("subshard-", seq_len(length(b)))
      return(b)
    })
    names(subworkflow_meta) <- subs
    # Likely needs some logic here to capture when subworkflows are found
    # but don't yet have calls to get metadata from
    sub_fails <- purrr::map_dfr(subworkflow_meta, function(subcall_data) {
      purrr::map_dfr(subcall_data, function(shard_data) {
        faildf <- purrr::map_dfr(subcall_data, function(call_data) {
          unlist(call_data)
        })
        faildf$workflow_id <- workflow_id
        return(faildf)
      })
    }, .id = "subWorkflowName")
  }

  if (length(bobfail) > 0) {
    faildf <- purrr::map(bobfail, function(call_data) {
      purrr::map_dfr(call_data, function(shard_data) {
        dplyr::as_tibble(rbind(unlist(shard_data)))
      })
    }) %>% purrr::map_dfr(., function(x) {
      x
    }, .id = "callName")
    faildf$workflow_id <- workflow_id
    temp1 <- dplyr::as_tibble(do.call(
      "rbind",
      strsplit(faildf$callName, split = "[.]")
    ))
    colnames(temp1) <- c("workflowName", "call")
    faildf <- cbind(faildf, temp1)
    faildf$callName <- NULL
    faildf <- dplyr::rename(faildf, "callName" = "call")

    if ("failures.message" %in% colnames(faildf)) {
      faildf <- dplyr::filter(faildf, !is.na(failures.message))
    } else {
      faildf <- faildf[0, ]
    }
    if (exists("sub_fails")) {
      faildf <- suppressMessages(dplyr::full_join(faildf, sub_fails))
    }
  } else {
    if (exists("sub_fails")) {
      faildf <- sub_fails
    } else {
      faildf <- dplyr::tibble()
    }
  }
  faildf
}
