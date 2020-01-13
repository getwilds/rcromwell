

#' Pull metadata for the calls made in a Cromwell workflow job
#'
#' Retrieve and process call metadata for a specific workflow.
#'
#' @param workflow_id The workflow ID to return call metadata on.
#' @return Returns a long form data frame of metadata on calls, does not currently support subWorkflows
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell URL to be set in the environment.
#' @examples
#' ## Set credentials from a file with the specified format, called `secrets.R` in path `~/myCreds/`.
#' setCreds(tokenSet = "file", path = "~/myCreds/secrets.R")
#' ## Request what jobs have been submitted to your Cromwell instance in the past 7 days.
#' recentJobs <- cromwellJobs(days = 7)
#' ## Request workflow metadata for a specific job that was run in your Cromwell instance.
#' thisWorkflowID <- recentJobs$workflow_id[1]
#' callsMeta <- cromwellCall(workflow_id = thisWorkflowID)
#' @export
cromwellCall <- function(workflow_id) {
  if ("" %in% Sys.getenv("CROMWELLURL")) {
    stop("CROMWELLURL is not set.")
  } else {
    print(paste0("Querying for call metadata for workflow id: ", workflow_id))
  }
  crommetadata <-
    httr::content(httr::GET(
      paste0(
        Sys.getenv("CROMWELLURL"),
        "/api/workflows/v1/",
        workflow_id,
        "/metadata?expandSubWorkflows=false"
      )
    ), as = "parsed")
  
  if(is.character(crommetadata) == T) stop(crommetadata)
  if (is.list(crommetadata$calls) == T) {
    # if the workflow has made calls, nab them
    bob <- purrr::pluck(crommetadata, "calls")
    if (length(bob) > 0) {
      # this is redudant but a better error catch isn't quite clear yet.
      justCalls <- purrr::map(bob, function(callData) {
        purrr::map_dfr(callData, function(shardData) {
          y <-
            purrr::discard(shardData, is.list) # only keep data that isn't a list itself!
          Z <- data.frame(rbind(unlist(y)), stringsAsFactors = F)
          
          if (shardData$executionStatus == "Failed") {
            failureData <-
              unlist(purrr::pluck(
                purrr::pluck(shardData, "failures", .default = NA),
                "causedBy",
                "message",
                .default = NA
              ))
            if (is.na(failureData) == F) {
              Zf <-
                data.frame(failureData[failureData != ""], stringsAsFactors = F)
              Z <- cbind(Z, Zf)
            }
          }
          
          if (is.null(shardData$runtimeAttributes) == F) {
            # if there are runtimeAttributes then..
            runTime <-
              purrr::pluck(shardData, "runtimeAttributes") # pull them out
            Z1 <-
              data.frame(rbind(unlist(runTime)), stringsAsFactors = F) # and make into a data frame
            Z <- cbind(Z, Z1) # put those together into just Z
          }
          return(Z)
        })
      }) %>% purrr::map_dfr(., function(x) {
        x
      }, .id = "callName") # melt it all down by callName
      
      justCalls$workflow_id <-
        workflow_id # This lets you take this output in a map_dfr and just do rbind as the function. ;)
      ## Big Chunk of dealing with start and end times.
      if ("start" %in% colnames(justCalls) == T) {
        # if the workflow has started
        justCalls$start <-
          as.POSIXct(justCalls$start, tz = "UTC", "%Y-%m-%dT%H:%M:%S") - 8*60*60 # because PST/hack
        if ("end" %in% colnames(justCalls) == T) {
          # and if end is present
          justCalls$end <-
            as.POSIXct(justCalls$end, tz = "UTC", "%Y-%m-%dT%H:%M:%S") - 8*60*60 # because PST/hack
          justCalls <-
            dplyr::mutate(justCalls, callDuration = round(difftime(end, start, units = "mins"), 3))
        } else {
          # if end doesn't exist or it is already NA (???), make it and workflowDuration but set to NA
          justCalls$end <- NA
          justCalls$callDuration <- NA
        }
      } else {
        # if start doesn't exist, then create it and set it to NA
        justCalls$start <- NA
      }
      justCalls <- dplyr::mutate_all(justCalls, as.character)
      justCalls$callDuration <- as.numeric(justCalls$callDuration)
      
    } else {
      justCalls <-
        data.frame("workflow_id" = "No call metadata available.", stringsAsFactors = F)
    }
  } else {
    justCalls <-
      data.frame("workflow_id" = "No call metadata available.", stringsAsFactors = F)
  }
  return(justCalls)
}