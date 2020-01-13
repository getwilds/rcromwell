#' Pull information about recent Cromwell workflow jobs
#'
#' Requests metadata about Cromwell workflow jobs during a time period specified.
#'
#' @param days The number of days of history to return, defaults to 1 day.
#' @return Returns a long form data frame of metadata on workflow jobs submitted to a specific Cromwell instance.
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell URL to be set in the environment.
#' @examples
#' ## Set credentials from a file with the specified format, called `secrets.R` in path `~/myCreds/`.
#' setCreds(tokenSet = "file", path = "~/myCreds/secrets.R")
#' ## Request what jobs have been submitted to your Cromwell instance in the past 7 days.
#' recentJobs <- cromwellJobs(days = 7)
#' @export
cromwellJobs <- function(days = 1) {
  if ("" %in% Sys.getenv("CROMWELLURL")) {
    stop("CROMWELLURL is not set.")
  } else
    print(paste0("Querying cromwell for jobs in the last ", days, " days."))
  beforeNow <- Sys.Date() - round(days, 0)
  cromDat <-
    httr::content(httr::GET(
      paste0(
        Sys.getenv("CROMWELLURL"),
        "/api/workflows/v1/query?submission=",
        beforeNow,
        "T00%3A00Z"
      )
    ))$results
  cromTable <- purrr::map_dfr(cromDat, dplyr::bind_rows)
  if (nrow(cromTable) > 0) {
    cromTable <- dplyr::rename(cromTable, "workflow_id" = "id")
    if ("end" %in% colnames(cromTable) == T &
        "start" %in% colnames(cromTable) == T) {
      cromTable$start <-
        as.POSIXct(cromTable$start, "UTC", "%Y-%m-%dT%H:%M:%S") - 8*60*60 # because PST/hack
      cromTable$end <-
        as.POSIXct(cromTable$end, "UTC", "%Y-%m-%dT%H:%M:%S") - 8*60*60 # because PST/hack
      cromTable$submission <-
        as.character(as.POSIXct(cromTable$submission, "UTC", "%Y-%m-%dT%H:%M:%S") - 8*60*60 )# because PST/hack)
      cromTable$workflowDuration <-
        round(difftime(cromTable$end, cromTable$start, units = "mins"),
              3)
      cromTable$workflowDuration <-
        as.numeric(cromTable$workflowDuration)
    } else {
      cromTable$workflowDuration <- "NA"
    }
  } else {
    cromTable <- data.frame("workflow_id" = NA,
                            stringsAsFactors = F)
  }
  return(cromTable)
}