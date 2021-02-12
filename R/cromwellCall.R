#' Retrieve metadata for the calls made in a Cromwell workflow job
#'
#' Retrieve and process call metadata for a specific workflow.
#'
#' @param workflow_id The workflow ID to return call metadata on.
#' @param cromURL The full string of the Cromwell URL to query if not using this locally (e.g. http://gizmog10:8000). (Optional)
#' @return Returns a long form data frame of metadata on calls. NOTE: does not currently support subWorkflows.
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell server URL to be set in the environment, or the use
#' of the cromURL param if you want to specify upon call the URL to use. (use `setCromwellURL()`)
#' Now supports nested scatters.
#' @examples
#' ## Request what jobs have been submitted to your Cromwell instance in the past 7 days.
#' recentJobs <- cromwellJobs(days = 7)
#' ## Request workflow metadata for a specific job that was run in your Cromwell instance.
#' thisWorkflowID <- recentJobs$workflow_id[1]
#' callsMeta <- cromwellCall(workflow_id = thisWorkflowID)
#' @export
cromwellCall <- function(workflow_id, cromURL = Sys.getenv("CROMWELLURL", unset = "needsURL")) {
  if(cromURL == "needsURL") {
    stop("CROMWELLURL is not set in your environment, or specify the URL to query via cromURL.")
  } else {
    message(paste0("Querying for call metadata for workflow id: ", workflow_id))
  }
  crommetadata <-
    httr::content(httr::GET(
      paste0(
        cromURL,
        "/api/workflows/v1/",
        workflow_id,
        "/metadata?expandSubWorkflows=true"
      )
    ), as = "parsed")
  # if the response is a character vector, then return it and stop
  if (is.character(crommetadata) == T) stop(crommetadata)
  # if the response is a list, meaning it has some content, continue
  if (is.list(crommetadata$calls) == T) {
    # if the workflow has made calls, nab them
    bob <- purrr::pluck(crommetadata, "calls")
    # If there is a scatter subworkflow, then take that data out and crunch it separately.
    if (sum(grepl("ScatterAt", names(bob))) > 0 ) {
      subs <- names(bob)[grepl("ScatterAt", names(bob))==T]
      subworkflow <- bob[subs]
      bob <- bob[!names(bob) %in% subs]
      subworkflowMeta <- purrr::map(subs, function(x) {
        b <- purrr::flatten(purrr::flatten(subworkflow[x]))
        c <- purrr::pluck(b, "subWorkflowMetadata")
        return(c)})

      names(subworkflowMeta) <- subs
      justSubCalls <- purrr::map(subworkflowMeta, function(subcallData) {
        calls <- purrr::pluck(subcallData, "calls")
        names(calls)
        out <- purrr::map_dfr(calls, function(taskData) {
          purrr::map_dfr(taskData, function(shards){
            # only keep data that isn't a list itself!
            y <- purrr::discard(shards, is.list)
            Z <- data.frame(rbind(unlist(y)), stringsAsFactors = F)
            # if the subworkflow failed, also return the failure metadata that is available
            if (shards$executionStatus == "Failed") {
              failureData <-
                unlist(purrr::pluck(
                  purrr::pluck(shards, "failures", .default = NA),
                  "causedBy", "message", .default = NA
                ))
              if (is.na(failureData) == F) {
                Zf <- data.frame(failureData[failureData != ""], stringsAsFactors = F)
                Z <- cbind(Z, Zf)
              }
            }
            if (is.null(shards$runtimeAttributes) == F) {
              # if there are runtimeAttributes then.. pull them out
              runTime <- purrr::pluck(shards, "runtimeAttributes")
              Z1 <-data.frame(rbind(unlist(runTime)), stringsAsFactors = F)
              Z <- cbind(Z, Z1)
            }
            return(Z)
          })
        }, .id = "fullName")
        return(out) })  %>% purrr::map_dfr(., function(x) { x }, .id = "subWorkflowName")
      # split fullname into workflowName and callName
      justSubCalls <- tidyr::separate(data = justSubCalls,
                                      col = fullName,
                                      into = c("workflowName", "callName"),
                                      sep = "\\.",
                                      extra = "merge")

      justSubCalls$workflow_id <- workflow_id # This lets you take this output in a map_dfr and just do rbind as the function. ;)
      ## Big Chunk of dealing with start and end times.
      if ("start" %in% colnames(justSubCalls) == T) {
        # if the workflow has started
        justSubCalls$start <- lubridate::with_tz(lubridate::ymd_hms(justSubCalls$start), tzone = "US/Pacific")
        if ("end" %in% colnames(justSubCalls) == T) {
          # and if end is present
          justSubCalls$end <-lubridate::with_tz(lubridate::ymd_hms(justSubCalls$end), tzone = "US/Pacific")
          justSubCalls <- justSubCalls %>% mutate(callDuration = ifelse(is.na(justSubCalls$end) == T,
                                                                        round(difftime(lubridate::now(tz = "US/Pacific"), justSubCalls$start, units = "mins"),3),
                                                                        round(difftime(justSubCalls$end, justSubCalls$start, units = "mins"), 3)))
        } else {
          # if end doesn't exist or it is already NA (???), make it and workflowDuration but set to NA
          justSubCalls$end <- NA
          justSubCalls$callDuration <- round(difftime(lubridate::now(tz = "US/Pacific"), justSubCalls$start, units = "mins"),3)
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
          Z <- data.frame(rbind(unlist(y)), stringsAsFactors = F)
          # if the workflow failed, also return the failure metadata that is available
          if (shardData$executionStatus == "Failed") {
            failureData <-
              unlist(purrr::pluck(
                purrr::pluck(shardData, "failures", .default = NA),
                "causedBy", "message", .default = NA
              ))
            if (is.na(failureData) == F) {
              Zf <- data.frame(failureData[failureData != ""], stringsAsFactors = F)
              Z <- cbind(Z, Zf)
            }
          }
          if (is.null(shardData$runtimeAttributes) == F) {
            # if there are runtimeAttributes then.. pull them out
            runTime <- purrr::pluck(shardData, "runtimeAttributes")
            Z1 <-data.frame(rbind(unlist(runTime)), stringsAsFactors = F)
            Z <- cbind(Z, Z1)
          }
          return(Z)
        })
      }) %>% purrr::map_dfr(., function(x) { x }, .id = "fullName")
      # melt it all down by callName

      # split fullname into workflowName and callName
      justCalls <- tidyr::separate(data = justCalls,
                                   col = fullName,
                                   into = c("workflowName", "callName"),
                                   sep = "\\.",
                                   extra = "merge")

      justCalls$workflow_id <- workflow_id # This lets you take this output in a map_dfr and just do rbind as the function. ;)
      ## Big Chunk of dealing with start and end times.
      if ("start" %in% colnames(justCalls) == T) {
        # if the workflow has started
        justCalls$start <- lubridate::with_tz(lubridate::ymd_hms(justCalls$start), tzone = "US/Pacific")
        if ("end" %in% colnames(justCalls) == T) {
          # and if end is present
          justCalls$end <-lubridate::with_tz(lubridate::ymd_hms(justCalls$end), tzone = "US/Pacific")
          justCalls <- justCalls %>% mutate(callDuration = ifelse(is.na(justCalls$end) == T,
                 round(difftime(lubridate::now(tz = "US/Pacific"), justCalls$start, units = "mins"),3),
                 round(difftime(justCalls$end, justCalls$start, units = "mins"), 3)))
        } else {
          # if end doesn't exist or it is already NA (???), make it and workflowDuration but set to NA
          justCalls$end <- NA
          justCalls$callDuration <- round(difftime(lubridate::now(tz = "US/Pacific"), justCalls$start, units = "mins"),3)
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
      justCalls <- dplyr::full_join(justCalls, justSubCalls)
    }
    } else {
      # returns a data frame if no data is avaialable so that this can be used with Shiny apps easier
      justCalls <- data.frame("workflow_id" = "No call metadata available.", stringsAsFactors = F)
    }
  } else {
    # returns a data frame if no data is avaialable so that this can be used with Shiny apps easierx
    justCalls <- data.frame("workflow_id" = "No call metadata available.", stringsAsFactors = F)
  }
  return(justCalls)
}
