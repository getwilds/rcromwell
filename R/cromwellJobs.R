#' Pull information about recent Cromwell workflow jobs
#'
#' Requests metadata about Cromwell workflow jobs during a time period specified.
#'
#' @param days The number of days of history to return, defaults to 1 day.
#' @param workflowName An array of strings of valid workflow names you want in your job list.
#' @param workflowStatus A array of strings of valid workflow statuses you want in your job list (e.g., submitted, running, succeeded, failed, aborting, aborted)
#' @param cromURL The full string of the Cromwell URL to query  (e.g. http://gizmog10:8000). (Optional)
#' @return Returns a long form data frame of metadata on workflow jobs submitted to a specific Cromwell instance.
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell server URL to be set in the environment, or the use
#' of the cromURL param if you want to specify upon call the URL to use.
#' @examples
#' ## Request what jobs have been submitted to your Cromwell instance in the past 7 days.
#' recentJobs <- cromwellJobs(days = 7)
#' @export
cromwellJobs <- function(days = 1, workflowName = NULL, workflowStatus = NULL,
                         cromURL = Sys.getenv("CROMWELLURL", unset = "needsURL")) {
  if(cromURL == "needsURL") {
    stop("CROMWELLURL is not set in your environment, or specify the URL to query via cromURL.")
  } else {
    message(paste0("Querying cromwell for jobs in the last ", days, " days."))
  }
  beforeNow <- Sys.Date() - round(days, 0)
  queryString <-paste0("submission=", beforeNow, "T00%3A00Z")
  if (is.null(workflowName)==F) {
    queryString <- paste(queryString, paste("name=", workflowName, sep = "", collapse = "&"), sep = "&")
  }
  if (is.null(workflowStatus) == F) {
    queryString <- paste(queryString, paste("status=", workflowStatus, sep = "", collapse = "&"), sep = "&")
  }
  cromDat <-
    httr::content(httr::GET(
      paste0(
        cromURL,
        "/api/workflows/v1/query?", queryString
      )
    ))$results
  cromTable <- purrr::map_dfr(cromDat, dplyr::bind_rows)
  if (nrow(cromTable) > 0 & "id" %in% names(cromTable)) {
    cromTable <- dplyr::rename(cromTable, "workflow_id" = "id")
    if ("name" %in% colnames(cromTable)) {cromTable <- dplyr::rename(cromTable, "workflowName" = "name")}
    cromTable$submission <- lubridate::with_tz(lubridate::ymd_hms(cromTable$submission), tzone = "US/Pacific")
    if ("start" %in% colnames(cromTable) == T) {
      cromTable$start <- lubridate::with_tz(lubridate::ymd_hms(cromTable$start), tzone = "US/Pacific") }
    if ("end" %in% colnames(cromTable) == T) {
      cromTable$end <-lubridate::with_tz(lubridate::ymd_hms(cromTable$end), tzone = "US/Pacific")
      cromTable$workflowDuration <- round(difftime(cromTable$end, cromTable$submission, units = "mins"),3)
      }
    if ("end" %in% colnames(cromTable) == F) {
      cromTable$workflowDuration <- round(difftime(lubridate::now(tz = "US/Pacific"), cromTable$submission, units = "mins"),3)}
    } else {
    cromTable <- data.frame("workflow_id" = NA,
                            stringsAsFactors = F) }
  convertToChar <- c("submission", "start", "end", "workflowDuration")
  theseCols <- colnames(cromTable) %in% convertToChar
  cromTable[theseCols] <- lapply(cromTable[theseCols], as.character)
  return(cromTable)
}
