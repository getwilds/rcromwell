#' Pull metadata for a specific Cromwell workflow job
#'
#' Retrieve and process all labels, submission and workflow level metadata for
#' a specific workflow.
#'
#' @template workflowid
#' @template serverdeets
#' @return Returns a long form data frame of metadata on a workflow.
#' @author Amy Paguirigan
#' @inheritSection workflow_options Important
#' @examples \dontrun{
#' ## Request what jobs have been submitted to your Cromwell instance in the
#' ## past 7 days.
#' recentJobs <- cromwell_jobs(days = 7)
#' ## Request workflow metadata for a specific job that was run in your
#' ## Cromwell instance.
#' thisWorkflowID <- recentJobs$workflow_id[1]
#' workflowMeta <- cromwell_workflow(workflow_id = thisWorkflowID)
#' }
#' @export
cromwell_workflow <- function(workflow_id, url = cw_url(), token = NULL) {
  check_url(url)
  crom_mssg(paste0("Querying for metadata for workflow id: ", workflow_id))

  meta <- http_get(
    url = make_url(url, "api/workflows/v1", workflow_id, "metadata"),
    query = list(expandSubWorkflows = "false", excludeKey = "calls"),
    as = "parsed",
    token = token
  )
  if (!is.list(meta)) {
    stop(
      "Likely the API timed out, please resubmit your request",
      " or check your Cromwell server."
    )
  }
  if (meta$status == "fail") {
    # this is when a workflow itself fails to start
    return(dplyr::tibble("workflow_id" = meta$message))
  } else {
    if ("id" %in% names(meta)) {
      # if a workflow starts then the id will be there
      # if a workflow has a list of labels
      if (is.list(meta$labels)) {
        drag <- purrr::pluck(meta, "labels")
        drag <- dplyr::as_tibble(purrr::flatten(drag))
        drag$workflow_id <- gsub("cromwell-", "", drag$`cromwell-workflow-id`)
        drag$`cromwell-workflow-id` <- NULL
      } else {
        drag <- dplyr::tibble("workflow_id" = workflow_id)
      }
      # Get submission data
      submit <- dplyr::as_tibble(purrr::flatten(purrr::pluck(
        meta,
        "submittedFiles"
      )))
      submit$labels <- NULL # why do they have labels here TOO!!?!
      submit$workflow_id <- workflow_id
      # Get remaining workflow level data
      remainder <- dplyr::as_tibble(purrr::discard(meta, is.list))
      remainder <- dplyr::rename(remainder, "workflow_id" = "id")
      # Get workflow failure data if it exists
      if (meta$status == "Failed") {
        failure_data <-
          unlist(purrr::pluck(
            purrr::pluck(meta, "failures", .default = NA),
            "causedBy",
            "message",
            .default = NA
          ))
        if (!is.na(failure_data)) {
          failures <- dplyr::as_tibble(failure_data[failure_data != ""])
          failures$workflow_id <- workflow_id
          resultdf <- purrr::reduce(list(remainder, drag, submit, failures),
            dplyr::full_join,
            by = "workflow_id"
          )
        } # if failures is na, then keep going
        resultdf <- purrr::reduce(list(remainder, drag, submit),
          dplyr::full_join,
          by = "workflow_id"
        )
      } else {
        resultdf <- purrr::reduce(list(remainder, drag, submit),
          dplyr::full_join,
          by = "workflow_id"
        )
      }
      resultdf$submission <-
        lubridate::with_tz(lubridate::ymd_hms(resultdf$submission),
          tzone = pkg_env$tzone
        )
      if ("start" %in% colnames(resultdf)) {
        resultdf$start <-
          lubridate::with_tz(lubridate::ymd_hms(resultdf$start),
            tzone = pkg_env$tzone
          )
      } else {
        # If the workflow hasn't started, then create the column but set to NA
        resultdf$start <- NA
      }
      if ("end" %in% colnames(resultdf)) {
        resultdf$end <- lubridate::with_tz(lubridate::ymd_hms(resultdf$end),
          tzone = pkg_env$tzone
        )
        resultdf$workflowDuration <-
          round(difftime(resultdf$end, resultdf$start, units = "mins"), 3)
      } else {
        # if end doesn't exist or it is already NA (???), make it and
        # workflowDuration but set to NA
        resultdf$end <- NA
        if (!is.na(resultdf$start)) {
          resultdf$workflowDuration <-
            round(difftime(Sys.time(), resultdf$start, units = "mins"), 3)
        } else {
          resultdf$workflowDuration <- 0
        }
      }
      resultdf <- dplyr::mutate_all(resultdf, as.character)
      resultdf$workflowDuration <- as.numeric(resultdf$workflowDuration)
    } else {
      # if id is not in the names, then
      resultdf <- dplyr::tibble("workflow_id" = "No metadata available.")
    }
  }
  return(resultdf)
}
