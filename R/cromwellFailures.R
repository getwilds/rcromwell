#' Pull metadata for the failed calls made in a Cromwell workflow job
#'
#' Gets info about failed calls for a specific workflow
#'
#' @param workflow_id The workflow ID to return call failure metadata for.
#' @return Returns a long form data frame of metadata on failed calls in a workflow.
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell server URL to be set in the environment. (use `setCromwellURL()`)
#' @examples
#' ## Request what jobs have been submitted to your Cromwell instance in the past 7 days.
#' recentJobs <- cromwellJobs(days = 7)
#' ## Request workflow metadata for a specific job that was run in your Cromwell instance.
#' thisWorkflowID <- recentJobs$workflow_id[1]
#' failsMeta <- cromwellFailures(workflow_id = thisWorkflowID)
#' @export
cromwellFailures <- function(workflow_id) {
  if ("" %in% Sys.getenv("CROMWELLURL")) {
    stop("CROMWELLURL is not set.")
  } else {
    message(paste0("Querying for failure metadata for workflow id: ", workflow_id))
  }
  cromfail <-
    httr::content(httr::GET(
      paste0(
        Sys.getenv("CROMWELLURL"),
        "/api/workflows/v1/",
        workflow_id,
        "/metadata?includeKey=failures&includeKey=jobId"
      )
    ), as = "parsed")
  if (is.list(cromfail$calls) == T) {
    bobfail <- purrr::pluck(cromfail, "calls")
    if (length(bobfail) > 0) {
      faildf <- purrr::map(bobfail, function(callData) {
        purrr::map_dfr(callData, function(shardData) {
          Z <- data.frame(rbind(unlist(shardData)), stringsAsFactors = F)
        })
      }) %>% purrr::map_dfr(., function(x) {
        x
      }, .id = "callName")
      faildf$workflow_id <- workflow_id
      # this section creates a URL for where the stderr log for the failed jobs likely are.
      # This is hardcoded for ALP right now b/c it's not part of what Cromwell returns.
      temp1 <- data.frame(do.call('rbind',strsplit(faildf$callName, split = "[.]")))
      colnames(temp1)<- c("workflowName", "call")
      faildf <- cbind(faildf, temp1)
      faildf$callName <- NULL
      faildf  <- dplyr::rename(faildf, "callName" = "call")
      if ("failures.message" %in% colnames(faildf)) {
        faildf <- dplyr::filter(faildf, is.na(failures.message) == F)
        callData <- cromwellCall(workflow_id = workflow_id)
        faildf <- suppressWarnings(dplyr::left_join(faildf, callData)) # come clean this
      } else {
        faildf <- faildf[0,]
      }
    } else {
      faildf <-
        data.frame("workflow_id" = "No failure metadata available.", stringsAsFactors = F)
    }
  } else {
    faildf <-
      data.frame("workflow_id" = "No failure metadata available.", stringsAsFactors = F)
  }
  return(faildf)
}
