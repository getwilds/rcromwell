#' Pull metadata for a specific Cromwell workflow job
#'
#' Retrieve and process all labels, submission and workflow level metadata for a specific workflow.
#'
#' @param workflow_id The workflow ID to return metadata for.
#' @return Returns a long form data frame of metadata on a workflow.
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell server URL to be set in the environment. (use `setCromwellURL()`)
#' @examples
#' ## Request what jobs have been submitted to your Cromwell instance in the past 7 days.
#' recentJobs <- cromwellJobs(days = 7)
#' ## Request workflow metadata for a specific job that was run in your Cromwell instance.
#' thisWorkflowID <- recentJobs$workflow_id[1]
#' workflowMeta <- cromwellWorkflow(workflow_id = thisWorkflowID)
#' @export
cromwellWorkflow <- function(workflow_id) {
  if ("" %in% Sys.getenv("CROMWELLURL")) {
    stop("CROMWELLURL is not set.")
  } else {
    print(paste0("Querying for metadata for workflow id: ", workflow_id))
  }
    crommetadata <-
      httr::content(httr::GET(
        paste0(
          Sys.getenv("CROMWELLURL"),
          "/api/workflows/v1/",
          workflow_id,
          "/metadata?expandSubWorkflows=false"
        )
      ), as = "parsed")

  if (crommetadata$status == "fail") {
    #this is when a workflow itself fails to start
    return(data.frame(
      "workflow_id" = crommetadata$message,
      stringsAsFactors = F
    ))
  } else {
    if ("id" %in% names(crommetadata)) {
      # if a workflow starts then the id will be there
      # if a workflow has a list of labels
      if (is.list(crommetadata$labels) == T) {
        drag <- purrr::pluck(crommetadata, "labels")
        drag <-
          data.frame(purrr::flatten(drag), stringsAsFactors = F)
        drag$workflow_id <-
          gsub("cromwell-", "", drag$cromwell.workflow.id)
        drag$cromwell.workflow.id <- NULL
      } else {
        drag <- data.frame("workflow_id" = workflow_id)
      }
      # Get submission data
      submit <-
        data.frame(purrr::flatten(purrr::pluck(crommetadata, "submittedFiles")), stringsAsFactors = F)
      submit$labels <-
        NULL # why do they have labels here TOO!!?!>!>
      submit$workflow_id <- workflow_id
      # Get remaining workflow level data
      remainder <-
        data.frame(purrr::discard(crommetadata, is.list),
                   stringsAsFactors = F)
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
        if (is.na(failureData) == F) {
          failures <-
            data.frame(failureData[failureData != ""], stringsAsFactors = F)
          failures$workflow_id <- workflow_id
          resultdf <-
            purrr::reduce(list(remainder, drag, submit, failures),
                          dplyr::full_join,
                          by = "workflow_id")
        } # if failures is na, then keep going
        resultdf <-
          purrr::reduce(list(remainder, drag, submit), dplyr::full_join, by = "workflow_id")
      } else {
        resultdf <-
          purrr::reduce(list(remainder, drag, submit), dplyr::full_join, by = "workflow_id")
      }
      #resultdf <- dplyr::mutate_all(resultdf, as.character)
      resultdf$submission <-
        as.character(as.POSIXct(resultdf$submission, tz= "America/Los_Angeles", "%Y-%m-%dT%H:%M:%S") - 7*60*60 )# because PST/hack)
      if ("start" %in% colnames(resultdf) == T) {
        # if the workflow has started
        if (is.na(resultdf$start) == F) {
          # and if the value of start is not NA
          resultdf$start <-
            as.POSIXct(resultdf$start, tz= "America/Los_Angeles", "%Y-%m-%dT%H:%M:%S") - 7*60*60 # because PST/hack
        } else {
          # if start is NA, then make sure it's set to NA????  Stupid.
          resultdf$start <- NA
        }
        if ("end" %in% colnames(resultdf) == T) {
          # and if end is present
          if (is.na(resultdf$end) == F) {
            # and it is not NA
            resultdf$end <-
              as.POSIXct(resultdf$end, tz= "America/Los_Angeles", "%Y-%m-%dT%H:%M:%S") - 7*60*60 # because PST/hack
            resultdf <-
              dplyr::mutate(resultdf, workflowDuration = round(difftime(end, start, units = "mins"), 3))
          }
        } else {
          # if end doesn't exist or it is already NA (???), make it and workflowDuration but set to NA
          resultdf$end <- NA
          if (is.na(resultdf$start)==F){resultdf$workflowDuration <- as.POSIXct(Sys.time())- resultdf$start} else {
            resultdf$workflowDuration <- 0}
        }
      } else {
        # if start doesn't exist, then create it and set it to NA
        resultdf$start <- NA
        # if start doesn't exist, then probably end and workflow Duration don't either.
        resultdf$end <- NA
        resultdf$workflowDuration <- 0
      }
      resultdf <- dplyr::mutate_all(resultdf, as.character)
      resultdf$workflowDuration <-
        as.numeric(resultdf$workflowDuration)
    } else {
      # if id is not in the names, then
      resultdf <-
        data.frame("workflow_id" = "No metadata available.", stringsAsFactors = F)
    }
    return(resultdf)
  }
}
