#' Requests metadata about Cromwell workflow jobs during a time period specified
#'
#' @export
#' @importFrom rlang set_names
#' @param days The number of days of history to return, defaults to 1 day.
#' @param workflow_name An array of strings of valid workflow names you want in
#' your job list.
#' @param workflow_status A array of strings of valid workflow statuses you want
#' in your job list (e.g., submitted, running, succeeded, failed,
#' aborting, aborted)
#' @author Amy Paguirigan, Scott Chamberlain
#' @inheritSection workflow_options Important
#' @return Returns a long form data frame of metadata on workflow jobs
#' submitted to a specific Cromwell instance.
#' @examples \dontrun{
#' ## Request what jobs have been submitted to your Cromwell instance in the
#' ## past 7 days.
#' recentJobs <- cromwell_jobs(days = 7)
#' }
cromwell_jobs <- function(days = 1,
                          workflow_name = NULL,
                          workflow_status = NULL) {
  check_url()
  crom_mssg(paste0("Querying cromwell for jobs in the last ", days, " days."))
  query <-
    list(submission = paste0(Sys.Date() - round(days, 0), "T00:00Z"))
  if (!is.null(workflow_name)) {
    query <- c(query, rlang::set_names(as.list(workflow_name), "name"))
  }
  if (!is.null(workflow_status)) {
    query <-
      c(query, rlang::set_names(as.list(workflow_status), "status"))
  }
  crom_dat <-
    http_get(make_url("api/workflows/v1/query"), query = query)$results
  cr_table <- purrr::map_dfr(crom_dat, dplyr::bind_rows)
  if (nrow(cr_table) > 0 && "id" %in% names(cr_table)) {
    cr_table <- dplyr::rename(cr_table, "workflow_id" = "id")
    if ("name" %in% colnames(cr_table)) {
      cr_table <- dplyr::rename(cr_table, "workflow_name" = "name")
    }
    cr_table$submission <- lubridate::with_tz(lubridate::ymd_hms(cr_table$submission),
      tzone = pkg_env$tzone
    )
    if ("start" %in% colnames(cr_table)) {
      cr_table$start <-
        lubridate::with_tz(lubridate::ymd_hms(cr_table$start),
          tzone = pkg_env$tzone
        )
    }
    if ("end" %in% colnames(cr_table)) {
      cr_table$end <- lubridate::with_tz(lubridate::ymd_hms(cr_table$end),
        tzone = pkg_env$tzone
      )
      cr_table$workflowDuration <- ifelse(is.na(cr_table$end),
        round(
          difftime(
            lubridate::now(tz = pkg_env$tzone),
            cr_table$submission,
            units = "mins"
          ),
          3
        ),
        round(
          difftime(cr_table$end, cr_table$submission, units = "mins"),
          3
        )
      )
    }
    if (!"end" %in% colnames(cr_table)) {
      cr_table$workflowDuration <-
        round(
          difftime(
            lubridate::now(tz = pkg_env$tzone),
            cr_table$submission,
            units = "mins"
          ),
          3
        )
    }
  } else {
    cr_table <- dplyr::tibble("workflow_id" = NA)
  }
  names_ <- c("submission", "start", "end", "workflowDuration")
  these_cols <- colnames(cr_table) %in% names_
  cr_table[these_cols] <- lapply(cr_table[these_cols], as.character)
  return(cr_table)
}
