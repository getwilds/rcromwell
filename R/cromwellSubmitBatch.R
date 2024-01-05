#' Submit a workflow job to Cromwell
#'
#' Supports the submission of a fully defined workflow job to a Cromwell
#' instance.
#'
#' @param wdl Local path to the wdl file describing the workflow. (Required)
#' @param params Local path to the json containing the parameters to use with
#' the workflow. (Optional)
#' @param batch Local path to the json containing a reference to any batch file
#' desired if the workflow is a batch. (Optional)
#' @param options Local path to the json containing workflow options to
#' apply. (Optional)
#' @param labels A data frame containing the labels for this workflow.
#' (Optional)
#' @param dependencies A zip'd file of subworkflow dependencies. (Optional)
#' @return Returns the response from the API post which includes the workflow
#' ID that you'll need to monitor the job.
#' @author Amy Paguirigan
#' @inheritSection workflowOptions Important
#' @export
cromwellSubmitBatch <-
  function(wdl, batch = NULL, params = NULL, options = NULL, labels = NULL,
           dependencies = NULL) {
    check_url()
    crom_mssg("Submitting a batch workflow to Cromwell")
    if (is.null(batch) && is.null(params)) {
      warning(
        "You did not submit either batch or params inputs.",
        "Was that on purpose?"
      )
    }
    bodyList <- list(
      workflowSource = httr::upload_file(wdl)
    )

    if (!is.null(params) && !is.null(batch)) {
      bodyList <- c(bodyList, workflowInputs = list(httr::upload_file(params)))
      bodyList <- c(bodyList, workflowInputs_2 = list(httr::upload_file(batch)))
    } else if (!is.null(params)) {
      bodyList <- c(bodyList, workflowInputs = list(httr::upload_file(params)))
    } else if (!is.null(batch)) {
      bodyList <- c(bodyList, workflowInputs = list(httr::upload_file(batch)))
    }

    if (!is.null(dependencies)) {
      bodyList <- c(bodyList,
        workflowDependencies = list(httr::upload_file(dependencies))
      )
    }
    if (!is.null(options)) {
      bodyList <- c(bodyList,
        workflowOptions = list(httr::upload_file(options))
      )
    }
    if (!is.null(labels)) {
      bodyList <- c(bodyList,
        labels = list(jsonlite::toJSON(as.list(labels), auto_unbox = TRUE))
      )
    }

    cromDat <-
      httpPOST(
        url = make_url("api/workflows/v1"),
        body = bodyList,
        encode = "multipart"
      )
    dplyr::as_tibble(cromDat)
  }
