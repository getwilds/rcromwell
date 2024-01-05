#' Pull metadata for the failed calls made in a Cromwell workflow job
#'
#' Gets info about failed calls for a specific workflow
#'
#' @template workflowid
#' @return Returns a long form data frame of metadata on failed calls in a
#' workflow.
#' @author Amy Paguirigan
#' @autoglobal
#' @inheritSection workflowOptions Important
#' @examples \dontrun{
#' ## Request what jobs have been submitted to your Cromwell instance in the
#' ## past 7 days.
#' recentJobs <- cromwellJobs(days = 7)
#' ## Request workflow metadata for a specific job that was run in your
#' ## Cromwell instance.
#' thisWorkflowID <- recentJobs$workflow_id[1]
#' failsMeta <- cromwellFailures(workflow_id = thisWorkflowID)
#' }
#' @export
cromwellFailures <- function(workflow_id) {
  check_url()
  crom_mssg(paste0(
    "Querying for failure metadata for workflow id: ",
    workflow_id
  ))

  cromfail <-
    httpGET(
      url = make_url("api/workflows/v1", workflow_id, "metadata"),
      query = list(includeKey = "failures", includeKey = "jobId"),
      as = "parsed"
    )
  if (is.list(cromfail$calls)) {
    bobfail <- purrr::pluck(cromfail, "calls")
    if (sum(grepl("ScatterAt", names(bobfail))) > 0) {
      subs <- names(bobfail)[grepl("ScatterAt", names(bobfail))]
      subworkflow <- bobfail[subs]
      bobfail <- bobfail[!names(bobfail) %in% subs]
      subworkflowMeta <- purrr::map(subs, function(x) {
        b <- purrr::flatten(subworkflow[x])
        names(b) <- paste0("subshard-", seq_len(length(b)))
        return(b)
      })
      names(subworkflowMeta) <- subs
      # Likely needs some logic here to capture when subworkflows are found
      # but don't yet have calls to get metadata from
      justSubFails <- purrr::map_dfr(subworkflowMeta, function(subcallData) {
        purrr::map_dfr(subcallData, function(shardData) {
          faildf <- purrr::map_dfr(subcallData, function(callData) {
            unlist(callData)
          })
          faildf$workflow_id <- workflow_id
          return(faildf)
        })
      }, .id = "subWorkflowName")
    }

    if (length(bobfail) > 0) {
      faildf <- purrr::map(bobfail, function(callData) {
        purrr::map_dfr(callData, function(shardData) {
          dplyr::as_tibble(rbind(unlist(shardData)))
        })
      }) %>% purrr::map_dfr(., function(x) {
        x
      }, .id = "callName")
      faildf$workflow_id <- workflow_id
      temp1 <- dplyr::as_tibble(do.call(
        "rbind",
        strsplit(faildf$callName, split = "[.]")
      ))
      colnames(temp1) <- c("workflowName", "call")
      faildf <- cbind(faildf, temp1)
      faildf$callName <- NULL
      faildf <- dplyr::rename(faildf, "callName" = "call")

      if ("failures.message" %in% colnames(faildf)) {
        faildf <- dplyr::filter(faildf, !is.na(failures.message))
      } else {
        faildf <- faildf[0, ]
      }
      if (exists("justSubFails")) {
        faildf <- suppressMessages(dplyr::full_join(faildf, justSubFails))
      }
    } else {
      if (exists("justSubFails")) {
        faildf <- justSubFails
      } else {
        faildf <- dplyr::tibble("workflow_id" = "No failure metadata available")
      }
    }
  } else {
    faildf <-
      dplyr::tibble("workflow_id" = "No failure metadata available.")
  }
  return(faildf)
}
