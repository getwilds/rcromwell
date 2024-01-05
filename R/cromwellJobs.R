#' Requests metadata about Cromwell workflow jobs during a time period specified
#'
#' @importFrom rlang set_names
#' @param days The number of days of history to return, defaults to 1 day.
#' @param workflowName An array of strings of valid workflow names you want in
#' your job list.
#' @param workflowStatus A array of strings of valid workflow statuses you want
#' in your job list (e.g., submitted, running, succeeded, failed,
#' aborting, aborted)
#' @return Returns a long form data frame of metadata on workflow jobs
#' submitted to a specific Cromwell instance.
#' @author Amy Paguirigan
#' @inheritSection workflowOptions Important
#' @examples \dontrun{
#' ## Request what jobs have been submitted to your Cromwell instance in the
#' ## past 7 days.
#' recentJobs <- cromwellJobs(days = 7)
#' }
#' @export
cromwellJobs <- function(days = 1, workflowName = NULL, workflowStatus = NULL) {
  check_url()
  crom_mssg(paste0("Querying cromwell for jobs in the last ", days, " days."))
  query <- list(submission = paste0(Sys.Date() - round(days, 0), "T00:00Z"))
  if (!is.null(workflowName)) {
    query <- c(query, rlang::set_names(as.list(workflowName), "name"))
  }
  if (!is.null(workflowStatus)) {
    query <- c(query, rlang::set_names(as.list(workflowStatus), "status"))
  }
  cromDat <-
    httpGET(make_url("api/workflows/v1/query"), query = query)$results
  cromTable <- purrr::map_dfr(cromDat, dplyr::bind_rows)
  if (nrow(cromTable) > 0 && "id" %in% names(cromTable)) {
    cromTable <- dplyr::rename(cromTable, "workflow_id" = "id")
    if ("name" %in% colnames(cromTable)) {
      cromTable <- dplyr::rename(cromTable, "workflowName" = "name")
    }
    cromTable$submission <- lubridate::with_tz(
      lubridate::ymd_hms(cromTable$submission),
      tzone = pkg_env$tzone
    )
    if ("start" %in% colnames(cromTable)) {
      cromTable$start <- lubridate::with_tz(lubridate::ymd_hms(cromTable$start),
        tzone = pkg_env$tzone
      )
    }
    if ("end" %in% colnames(cromTable)) {
      cromTable$end <- lubridate::with_tz(lubridate::ymd_hms(cromTable$end),
        tzone = pkg_env$tzone
      )
      cromTable$workflowDuration <- ifelse(is.na(cromTable$end),
        round(difftime(
          lubridate::now(tz = pkg_env$tzone),
          cromTable$submission,
          units = "mins"
        ), 3),
        round(difftime(cromTable$end, cromTable$submission, units = "mins"), 3)
      )
    }
    if (!"end" %in% colnames(cromTable)) {
      cromTable$workflowDuration <-
        round(difftime(lubridate::now(tz = pkg_env$tzone),
          cromTable$submission,
          units = "mins"
        ), 3)
    }
  } else {
    cromTable <- dplyr::tibble("workflow_id" = NA)
  }
  convertToChar <- c("submission", "start", "end", "workflowDuration")
  theseCols <- colnames(cromTable) %in% convertToChar
  cromTable[theseCols] <- lapply(cromTable[theseCols], as.character)
  return(cromTable)
}
