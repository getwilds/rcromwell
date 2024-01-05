#' Retrieve metadata for the calls made in a Cromwell workflow job
#'
#' Retrieve and process call metadata for a specific workflow.
#'
#' @template workflowid
#' @return Returns a long form data frame of metadata on calls.
#' NOTE: does not currently support subWorkflows well yet.
#' @author Amy Paguirigan
#' @inheritSection workflowOptions Important
#' @details Now supports nested scatters.
#' @autoglobal
#' @examples \dontrun{
#' ## Request what jobs have been submitted to your Cromwell instance in the
#' ## past 7 days.
#' recentJobs <- cromwellJobs(days = 7)
#' ## Request workflow metadata for a specific job that was run in your
#' ## Cromwell instance.
#' thisWorkflowID <- recentJobs$workflow_id[1]
#' callsMeta <- cromwellCall(workflow_id = thisWorkflowID)
#' }
#' @export
cromwellCall <- function(workflow_id) {
  check_url()
  crom_mssg(paste0("Querying for call metadata for workflow id: ", workflow_id))
  crommetadata <-
    httpGET(
      url = make_url("api/workflows/v1", workflow_id, "metadata"),
      query = list(expandSubWorkflows = "true"),
      as = "parsed"
    )
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
      subworkflowMeta <- purrr::map(subs, function(x) {
        b <- purrr::flatten(subworkflow[x])
        names(b) <- paste0("subshard-", seq_len(length(b)))
        return(b)
      })
      names(subworkflowMeta) <- subs
      # Likely needs some logic here to capture when subworkflows are found but
      # don't yet have calls to get metadata from
      justSubCalls <- purrr::map_dfr(subworkflowMeta, function(subcallData) {
        purrr::map_dfr(subcallData, function(shardData) {
          calls <- shardData$subWorkflowMetadata$calls
          names(calls)
          out <- purrr::map_dfr(calls, function(taskData) {
            purrr::map_dfr(taskData, function(shards) {
              # only keep data that isn't a list itself!
              y <- purrr::discard(shards, is.list)
              Z <- dplyr::as_tibble(rbind(unlist(y)))
              # if the subworkflow failed, also return the failure metadata
              # that is available
              if (shards$executionStatus == "Failed") {
                failureData <-
                  unlist(purrr::pluck(
                    purrr::pluck(shards, "failures", .default = NA),
                    "causedBy", "message",
                    .default = NA
                  ))
                if (!is.na(failureData)) {
                  Zf <- dplyr::as_tibble(failureData[failureData != ""])
                  Z <- cbind(Z, Zf)
                }
              }
              if (!is.null(shards$runtimeAttributes)) {
                # if there are runtimeAttributes then.. pull them out
                runTime <- purrr::pluck(shards, "runtimeAttributes")
                Z1 <- dplyr::as_tibble(rbind(unlist(runTime)))
                Z <- cbind(Z, Z1)
              }
              return(Z)
            })
          }, .id = "fullName")
          return(out)
        }) %>% purrr::map_dfr(., function(x) {
          x
        }, .id = "subWorkflowName")
      }, .id = "detailedSubName")
      # split fullname into workflowName and callName
      justSubCalls <- tidyr::separate(
        data = justSubCalls,
        col = fullName,
        into = c("workflowName", "callName"),
        sep = "\\.",
        extra = "merge"
      )
      # This lets you take this output in a map_dfr and just do rbind
      # as the function
      justSubCalls$workflow_id <- workflow_id
      ## Big Chunk of dealing with start and end times.
      if ("start" %in% colnames(justSubCalls)) {
        # if the workflow has started
        justSubCalls$start <- lubridate::with_tz(
          lubridate::ymd_hms(justSubCalls$start),
          tzone = pkg_env$tzone
        )
        if ("end" %in% colnames(justSubCalls)) {
          # and if end is present
          justSubCalls$end <- lubridate::with_tz(
            lubridate::ymd_hms(justSubCalls$end),
            tzone = pkg_env$tzone
          )
          justSubCalls <- justSubCalls %>%
            dplyr::mutate(callDuration = ifelse(is.na(justSubCalls$end),
              round(difftime(lubridate::now(tz = pkg_env$tzone),
                justSubCalls$start,
                units = "mins"
              ), 3),
              round(difftime(justSubCalls$end,
                justSubCalls$start,
                units = "mins"
              ), 3)
            ))
        } else {
          # if end doesn't exist or it is already NA (???), make it and
          # workflowDuration but set to NA
          justSubCalls$end <- NA
          justSubCalls$callDuration <- round(
            difftime(lubridate::now(tz = pkg_env$tzone),
              justSubCalls$start,
              units = "mins"
            ), 3
          )
        }
      } else {
        # if start doesn't exist, then create it and set it to NA
        justSubCalls$start <- NA
        justSubCalls$end <- NA
        justSubCalls$callDuration <- 0
      }
      justSubCalls <- dplyr::mutate_all(justSubCalls, as.character)
      justSubCalls$callDuration <- as.numeric(justSubCalls$callDuration)
      justSubCalls$commandLine <- NULL # usually we don't need the command line
    }

    if (length(bob) > 0) {
      # this is redundant but a better error catch isn't quite clear yet.
      justCalls <- purrr::map(bob, function(callData) {
        purrr::map_dfr(callData, function(shardData) {
          # only keep data that isn't a list itself!
          y <- purrr::discard(shardData, is.list)
          Z <- dplyr::as_tibble(rbind(unlist(y)))
          # if the workflow failed, also return the failure metadata that
          # is available
          if (shardData$executionStatus == "Failed") {
            failureData <-
              unlist(purrr::pluck(
                purrr::pluck(shardData, "failures", .default = NA),
                "causedBy", "message",
                .default = NA
              ))
            if (!is.na(failureData)) {
              Zf <- dplyr::as_tibble(failureData[failureData != ""])
              Z <- cbind(Z, Zf)
            }
          }
          if (!is.null(shardData$runtimeAttributes)) {
            # if there are runtimeAttributes then.. pull them out
            runTime <- purrr::pluck(shardData, "runtimeAttributes")
            Z1 <- dplyr::as_tibble(rbind(unlist(runTime)))
            Z <- cbind(Z, Z1)
          }
          return(Z)
        })
      }) %>% purrr::map_dfr(., function(x) {
        x
      }, .id = "fullName")
      # melt it all down by callName

      # split fullname into workflowName and callName
      justCalls <- tidyr::separate(
        data = justCalls,
        col = fullName,
        into = c("workflowName", "callName"),
        sep = "\\.",
        extra = "merge"
      )

      # This lets you take this output in a map_dfr and just do rbind as
      # the function
      justCalls$workflow_id <- workflow_id
      ## Big Chunk of dealing with start and end times.
      if ("start" %in% colnames(justCalls)) {
        # if the workflow has started
        justCalls$start <-
          lubridate::with_tz(lubridate::ymd_hms(justCalls$start),
            tzone = pkg_env$tzone
          )
        if ("end" %in% colnames(justCalls)) {
          # and if end is present
          justCalls$end <- lubridate::with_tz(lubridate::ymd_hms(justCalls$end),
            tzone = pkg_env$tzone
          )
          justCalls <- justCalls %>%
            dplyr::mutate(callDuration = ifelse(is.na(justCalls$end),
              round(difftime(lubridate::now(tz = pkg_env$tzone),
                justCalls$start,
                units = "mins"
              ), 3),
              round(difftime(justCalls$end, justCalls$start, units = "mins"), 3)
            ))
        } else {
          # if end doesn't exist or it is already NA (???), make it and
          # workflowDuration but set to NA
          justCalls$end <- NA
          justCalls$callDuration <-
            round(difftime(lubridate::now(tz = pkg_env$tzone),
              justCalls$start,
              units = "mins"
            ), 3)
        }
      } else {
        # if start doesn't exist, then create it and set it to NA
        justCalls$start <- NA
        justCalls$end <- NA
        justCalls$callDuration <- 0
      }
      justCalls <- dplyr::mutate_all(justCalls, as.character)
      justCalls$callDuration <- as.numeric(justCalls$callDuration)

      if (exists("justSubCalls")) {
        justCalls <- suppressMessages(dplyr::full_join(justCalls, justSubCalls))
      }
    } else if (exists("justSubCalls")) {
      justCalls <- justSubCalls
    } else {
      # returns a data frame if no data is available so that this can be used
      # with Shiny apps easier
      justCalls <- dplyr::tibble("workflow_id" = "No call metadata available.")
    }
  } else {
    # returns a data frame if no data is avaialable so that this can be used
    # with Shiny apps easier
    justCalls <- dplyr::tibble("workflow_id" = "No call metadata available.")
  }
  return(justCalls)
}
