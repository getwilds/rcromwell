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
#' @template serverdeets
#' @author Amy Paguirigan, Scott Chamberlain
#' @inheritSection workflow_options Important
#' @return a tibble of metadata on workflow jobs submitted; each row is a
#' different job. columns:
#' - end (dttm)
#' - workflow_id (chr)
#' - metadataArchiveStatus (chr)
#' - workflow_name (chr)
#' - start (dttm)
#' - status (chr)
#' - submission (dttm)
#' - workflowDuration (dbl)
#' @examples \dontrun{
#' ## Request what jobs have been submitted to your Cromwell instance in the
#' ## past 7 days.
#' recentJobs <- cromwell_jobs(days = 7)
#' }
cromwell_jobs <- function(days = 1,
                          workflow_name = NULL,
                          workflow_status = NULL,
                          url = cw_url(),
                          token = NULL) {
  check_url(url)
  crom_mssg(glue("Querying cromwell for jobs in the last {days} days"))
  query <- cromwell_jobs_query(days, workflow_name, workflow_status)
  jobs_data <- http_get(
    make_url(url, "api/workflows/v1/query"),
    query = query, token = token
  )
  cromwell_jobs_process(jobs_data$results)
}

cromwell_jobs_query <- function(days, workflow_name, workflow_status) {
  query <-
    list(submission = paste0(Sys.Date() - round(days, 0), "T00:00Z"))
  if (!is.null(workflow_name)) {
    query <- c(query, rlang::set_names(as.list(workflow_name), "name"))
  }
  if (!is.null(workflow_status)) {
    query <-
      c(query, rlang::set_names(as.list(workflow_status), "status"))
  }
  query
}

cromwell_jobs_process <- function(jobs_data) {
  cr_table <- purrr::map_dfr(jobs_data, dplyr::bind_rows)
  if (nrow(cr_table) > 0 && "id" %in% names(cr_table)) {
    cr_table <- dplyr::rename(cr_table, "workflow_id" = "id")
    if ("name" %in% colnames(cr_table)) {
      cr_table <- dplyr::rename(cr_table, "workflow_name" = "name")
    }
    cr_table$submission <- lubridate::with_tz(
      lubridate::ymd_hms(cr_table$submission),
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

  # specific column order if columns exist (via `any_of`)
  columns_order <- c("workflow_name", "workflow_id", "status", "submission", "start",
    "end", "workflowDuration")
  cr_table <- dplyr::relocate(cr_table, any_of(columns_order))
  return(cr_table)
}
