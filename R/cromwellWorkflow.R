#' Pull metadata for a specific Cromwell workflow job
#'
#' Retrieve and process all labels, submission and workflow level metadata for
#' a specific workflow.
#'
#' @template workflowid
#' @return Returns a long form data frame of metadata on a workflow.
#' @author Amy Paguirigan
#' @inheritSection workflowOptions Important
#' @examples \dontrun{
#' ## Request what jobs have been submitted to your Cromwell instance in the
#' ## past 7 days.
#' recentJobs <- cromwellJobs(days = 7)
#' ## Request workflow metadata for a specific job that was run in your
#' ## Cromwell instance.
#' thisWorkflowID <- recentJobs$workflow_id[1]
#' workflowMeta <- cromwellWorkflow(workflow_id = thisWorkflowID)
#' }
#' @export
cromwellWorkflow <- function(workflow_id) {
  check_url()
  crom_mssg(paste0("Querying for metadata for workflow id: ", workflow_id))

  crommetadata <- httpGET(
    url = make_url("api/workflows/v1", workflow_id, "metadata"),
    query = list(expandSubWorkflows = "false", excludeKey = "calls"),
    as = "parsed"
  )
  if (!is.list(crommetadata)) {
    stop(
      "Likely the API timed out, please resubmit your request",
      " or check your Cromwell server."
    )
  }
  if (crommetadata$status == "fail") {
    # this is when a workflow itself fails to start
    return(dplyr::tibble("workflow_id" = crommetadata$message))
  } else {
    if ("id" %in% names(crommetadata)) {
      # if a workflow starts then the id will be there
      # if a workflow has a list of labels
      if (is.list(crommetadata$labels)) {
        drag <- purrr::pluck(crommetadata, "labels")
        drag <- dplyr::as_tibble(purrr::flatten(drag))
        drag$workflow_id <- gsub("cromwell-", "", drag$`cromwell-workflow-id`)
        drag$`cromwell-workflow-id` <- NULL
      } else {
        drag <- dplyr::tibble("workflow_id" = workflow_id)
      }
      # Get submission data
      submit <- dplyr::as_tibble(purrr::flatten(purrr::pluck(
        crommetadata,
        "submittedFiles"
      )))
      submit$labels <- NULL # why do they have labels here TOO!!?!
      submit$workflow_id <- workflow_id
      # Get remaining workflow level data
      remainder <- dplyr::as_tibble(purrr::discard(crommetadata, is.list))
      remainder <- dplyr::rename(remainder, "workflow_id" = "id")
      # Get workflow failure data if it exists
      if (crommetadata$status == "Failed") {
        failureData <-
          unlist(purrr::pluck(
            purrr::pluck(crommetadata, "failures", .default = NA),
            "causedBy",
            "message",
            .default = NA
          ))
        if (!is.na(failureData)) {
          failures <- dplyr::as_tibble(failureData[failureData != ""])
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
