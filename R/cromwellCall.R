# nolint start

#' Retrieve metadata for the calls made in a Cromwell workflow job
#'
#' Retrieve and process call metadata for a specific workflow.
#'
#' @export
#' @template workflowid
#' @template serverdeets
#' @importFrom tidyr separate
#' @inheritSection workflow_options Important
#' @details Now supports nested scatters.
#' @autoglobal
#' @author Amy Paguirigan, Scott Chamberlain
#' @return Returns a long form data frame of metadata on calls.
#' NOTE: does not currently support subWorkflows well yet.
#' @examples \dontrun{
#' ## Request what jobs have been submitted to your Cromwell instance in the
#' ## past 7 days.
#' recentJobs <- cromwell_jobs(days = 7)
#' ## Request workflow metadata for a specific job that was run in your
#' ## Cromwell instance.
#' thisWorkflowID <- recentJobs$workflow_id[1]
#' callsMeta <- cromwell_call(workflow_id = thisWorkflowID)
#' }
cromwell_call <- function(workflow_id, url = cw_url(), token = NULL) {
  check_url(url)
  crom_mssg(glue("Querying for call metadata for workflow id: {workflow_id}"))
  crommetadata <-
    http_req_get(
      url = make_url(url, "api/workflows/v1", workflow_id, "metadata"),
      token = token
    ) |>
    req_url_query(expandSubWorkflows = "true") |>
    http_perform()
  # if the response is a character vector, then return it and stop
  if (is.character(crommetadata)) stop(crommetadata)
  # if the response is a list, meaning it has some content, continue
  if (is.list(crommetadata$calls)) {
    # if the workflow has made calls, nab them
    bob <- purrr::pluck(crommetadata, "calls")
    # If there is a scatter subworkflow, then take that data out and crunch it
    # separately.
    if (sum(grepl("ScatterAt", names(bob))) > 0) {
      subs <- names(bob)[grepl("ScatterAt", names(bob))]
      subworkflow <- bob[subs]
      bob <- bob[!names(bob) %in% subs]
      sub_wf_meta <- purrr::map(subs, function(x) {
        b <- purrr::flatten(subworkflow[x])
        names(b) <- paste0("subshard-", seq_len(length(b)))
        return(b)
      })
      names(sub_wf_meta) <- subs
      # Likely needs some logic here to capture when subworkflows are found but
      # don't yet have calls to get metadata from
      just_sub_calls <- purrr::map_dfr(sub_wf_meta, function(subcall_data) {
        purrr::map_dfr(subcall_data, function(shard_data) {
          calls <- shard_data$subWorkflowMetadata$calls
          names(calls)
          out <- purrr::map_dfr(calls, function(task_data) {
            purrr::map_dfr(task_data, function(shards) {
              # only keep data that isn't a list itself!
              y <- purrr::discard(shards, is.list)
              z <- dplyr::as_tibble(rbind(unlist(y)))
              # if the subworkflow failed, also return the failure metadata
              # that is available
              if (shards$executionStatus == "Failed") {
                failure_data <-
                  unlist(purrr::pluck(
                    purrr::pluck(shards, "failures", .default = NA),
                    "causedBy", "message",
                    .default = NA
                  ))
                if (!is.na(failure_data)) {
                  zf <- dplyr::as_tibble(failure_data[failure_data != ""])
                  z <- cbind(z, zf)
                }
              }
              if (!is.null(shards$runtimeAttributes)) {
                # if there are runtimeAttributes then.. pull them out
                run_time <- purrr::pluck(shards, "runtimeAttributes")
                z1 <- dplyr::as_tibble(rbind(unlist(run_time)))
                z <- cbind(z, z1)
              }
              return(z)
            })
          }, .id = "fullName")
          return(out)
        }) %>% purrr::map_dfr(., function(x) {
          x
        }, .id = "subWorkflowName")
      }, .id = "detailedSubName")
      # split fullname into workflowName and callName
      just_sub_calls <- tidyr::separate(
        data = just_sub_calls,
        col = fullName,
        into = c("workflowName", "callName"),
        sep = "\\.",
        extra = "merge"
      )
      # This lets you take this output in a map_dfr and just do rbind
      # as the function
      just_sub_calls$workflow_id <- workflow_id
      ## Big Chunk of dealing with start and end times.
      if ("start" %in% colnames(just_sub_calls)) {
        # if the workflow has started
        just_sub_calls$start <- lubridate::with_tz(
          lubridate::ymd_hms(just_sub_calls$start),
          tzone = pkg_env$tzone
        )
        if ("end" %in% colnames(just_sub_calls)) {
          # and if end is present
          just_sub_calls$end <- lubridate::with_tz(
            lubridate::ymd_hms(just_sub_calls$end),
            tzone = pkg_env$tzone
          )
          just_sub_calls <- just_sub_calls %>%
            dplyr::mutate(callDuration = ifelse(is.na(just_sub_calls$end),
              round(difftime(lubridate::now(tz = pkg_env$tzone),
                just_sub_calls$start,
                units = "mins"
              ), 3),
              round(difftime(just_sub_calls$end,
                just_sub_calls$start,
                units = "mins"
              ), 3)
            ))
        } else {
          # if end doesn't exist or it is already NA (???), make it and
          # workflowDuration but set to NA
          just_sub_calls$end <- NA
          just_sub_calls$callDuration <- round(
            difftime(lubridate::now(tz = pkg_env$tzone),
              just_sub_calls$start,
              units = "mins"
            ), 3
          )
        }
      } else {
        # if start doesn't exist, then create it and set it to NA
        just_sub_calls$start <- NA
        just_sub_calls$end <- NA
        just_sub_calls$callDuration <- 0
      }
      just_sub_calls <- dplyr::mutate_all(just_sub_calls, as.character)
      just_sub_calls$callDuration <- as.numeric(just_sub_calls$callDuration)
      just_sub_calls$commandLine <- NULL
    }

    if (length(bob) > 0) {
      # this is redundant but a better error catch isn't quite clear yet.
      just_calls <- purrr::map(bob, function(call_data) {
        purrr::map_dfr(call_data, function(shard_data) {
          # only keep data that isn't a list itself!
          y <- purrr::discard(shard_data, is.list)
          z <- dplyr::as_tibble(rbind(unlist(y)))
          # if the workflow failed, also return the failure metadata that
          # is available
          if (shard_data$executionStatus == "Failed") {
            failure_data <-
              unlist(purrr::pluck(
                purrr::pluck(shard_data, "failures", .default = NA),
                "causedBy", "message",
                .default = NA
              ))
            if (!is.na(failure_data)) {
              zf <- dplyr::as_tibble(failure_data[failure_data != ""])
              z <- cbind(z, zf)
            }
          }
          if (!is.null(shard_data$runtimeAttributes)) {
            # if there are runtimeAttributes then.. pull them out
            run_time <- purrr::pluck(shard_data, "runtimeAttributes")
            z1 <- dplyr::as_tibble(rbind(unlist(run_time)))
            z <- cbind(z, z1)
          }
          return(z)
        })
      }) %>% purrr::map_dfr(., function(x) {
        x
      }, .id = "fullName")
      # melt it all down by callName

      # split fullname into workflowName and callName
      just_calls <- tidyr::separate(
        data = just_calls,
        col = fullName,
        into = c("workflowName", "callName"),
        sep = "\\.",
        extra = "merge"
      )

      # This lets you take this output in a map_dfr and just do rbind as
      # the function
      just_calls$workflow_id <- workflow_id
      ## Big Chunk of dealing with start and end times.
      if ("start" %in% colnames(just_calls)) {
        # if the workflow has started
        just_calls$start <-
          lubridate::with_tz(lubridate::ymd_hms(just_calls$start),
            tzone = pkg_env$tzone
          )
        if ("end" %in% colnames(just_calls)) {
          # and if end is present
          just_calls$end <-
            lubridate::with_tz(lubridate::ymd_hms(just_calls$end),
              tzone = pkg_env$tzone
            )
          just_calls <- just_calls %>%
            dplyr::mutate(callDuration = ifelse(is.na(just_calls$end),
              round(difftime(lubridate::now(tz = pkg_env$tzone),
                just_calls$start,
                units = "mins"
              ), 3),
              round(difftime(just_calls$end,
                just_calls$start,
                units = "mins"
              ), 3)
            ))
        } else {
          # if end doesn't exist or it is already NA (???), make it and
          # workflowDuration but set to NA
          just_calls$end <- NA
          just_calls$callDuration <-
            round(difftime(lubridate::now(tz = pkg_env$tzone),
              just_calls$start,
              units = "mins"
            ), 3)
        }
      } else {
        # if start doesn't exist, then create it and set it to NA
        just_calls$start <- NA
        just_calls$end <- NA
        just_calls$callDuration <- 0
      }
      just_calls <- dplyr::mutate_all(just_calls, as.character)
      just_calls$callDuration <- as.numeric(just_calls$callDuration)

      if (exists("just_sub_calls")) {
        just_calls <-
          suppressMessages(dplyr::full_join(just_calls, just_sub_calls))
      }
    } else if (exists("just_sub_calls")) {
      just_calls <- just_sub_calls
    } else {
      # returns a data frame if no data is available so that this can be used
      # with Shiny apps easier
      just_calls <- dplyr::tibble("workflow_id" = "No call metadata available.")
    }
  } else {
    # returns a data frame if no data is avaialable so that this can be used
    # with Shiny apps easier
    just_calls <- dplyr::tibble("workflow_id" = "No call metadata available.")
  }
  return(just_calls)
}
# nolint end
