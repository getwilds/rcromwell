#' Pull information about recent Cromwell workflow jobs
#'
#' Requests metadata about Cromwell workflow jobs during a time period specified.
#'
#' @param days The number of days of history to return, defaults to 1 day.
#' @param workflowName An array of strings of valid workflow names you want in your job list.
#' @param workflowStatus A array of strings of valid workflow statuses you want in your job list (e.g., submitted, running, suceeded, failed, aborting, aborted)
#' @return Returns a long form data frame of metadata on workflow jobs submitted to a specific Cromwell instance.
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell server URL to be set in the environment. (use `setCromwellURL()`)
#' @examples
#' ## Request what jobs have been submitted to your Cromwell instance in the past 7 days.
#' recentJobs <- cromwellJobs(days = 7)
#' @export
cromwellJobs <- function(days = 1, workflowName = NULL, workflowStatus = NULL) {
  if ("" %in% Sys.getenv("CROMWELLURL")) {
    stop("CROMWELLURL is not set.")
  } else
    message(paste0("Querying cromwell for jobs in the last ", days, " days."))
  beforeNow <- Sys.Date() - round(days, 0)
  queryString <-paste0("submission=", beforeNow, "T00%3A00Z"); queryString
  if (is.null(workflowName)==F){
    queryString <- paste(queryString, paste("name=", workflowName, sep = "", collapse = "&"), sep = "&")
  }
  if (is.null(workflowStatus) == F) {
    queryString <- paste(queryString, paste("status=", workflowStatus, sep = "", collapse = "&"), sep = "&")
  }
  print(queryString)
  cromDat <-
    httr::content(httr::GET(
      paste0(
        Sys.getenv("CROMWELLURL"),
        "/api/workflows/v1/query?", queryString
      )
    ))$results
  cromTable <- purrr::map_dfr(cromDat, dplyr::bind_rows)
  if (nrow(cromTable) > 0) {
    cromTable <- dplyr::rename(cromTable, "workflow_id" = "id")
    if ("end" %in% colnames(cromTable) == T &
        "start" %in% colnames(cromTable) == T) {
      cromTable$start <-lubridate::with_tz(lubridate::ymd_hms(cromTable$start), tzone = "US/Pacific")
      cromTable$end <-lubridate::with_tz(lubridate::ymd_hms(cromTable$end), tzone = "US/Pacific")
      cromTable$submission <-lubridate::with_tz(lubridate::ymd_hms(cromTable$submission), tzone = "US/Pacific")
      cromTable$workflowDuration <-
        round(difftime(cromTable$end, cromTable$start, units = "mins"),
              3)
      cromTable$workflowDuration <-
        as.numeric(cromTable$workflowDuration)
    } else if ("start" %in% colnames(cromTable) == T) {
      cromTable$start <-lubridate::with_tz(lubridate::ymd_hms(cromTable$start), tzone = "US/Pacific")
      # If it hasn't ended yet, then report the amount of time it's been running to that moment.
      cromTable$workflowDuration <- round(difftime(Sys.time(), cromTable$start, units = "mins"),3)
    } else {
      cromTable$workflowDuration <- 0
    }
  } else {
    cromTable <- data.frame("workflow_id" = NA,
                            stringsAsFactors = F)
  }
  return(cromTable)
}
