#' Submit a workflow job to Cromwell
#'
#' Supports the submission of a fully defined workflow job to a Cromwell
#' instance.
#'
#' @export
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
#' @template serverdeets
#' @inheritSection workflow_options Important
#' @author Amy Paguirigan, Scott Chamberlain
#' @return Returns the response from the API post which includes the workflow
#' ID that you'll need to monitor the job.
cromwell_submit_batch <-
  function(wdl, batch = NULL, params = NULL, options = NULL, labels = NULL,
           dependencies = NULL, url = cw_url(), token = NULL) {
    check_url(url)
    crom_mssg("Submitting a batch workflow to Cromwell")
    if (is.null(batch) && is.null(params)) {
      warning(
        "You did not submit either batch or params inputs.",
        " Was that on purpose?"
      )
    }
    body <- list(
      workflowSource = httr::upload_file(wdl)
    )

    if (!is.null(params) && !is.null(batch)) {
      body <- c(body, workflowInputs = list(httr::upload_file(params)))
      body <- c(body, workflowInputs_2 = list(httr::upload_file(batch)))
    } else if (!is.null(params)) {
      body <- c(body, workflowInputs = list(httr::upload_file(params)))
    } else if (!is.null(batch)) {
      body <- c(body, workflowInputs = list(httr::upload_file(batch)))
    }

    if (!is.null(dependencies)) {
      body <- c(body,
        workflowDependencies = list(httr::upload_file(dependencies))
      )
    }
    if (!is.null(options)) {
      body <- c(body,
        workflowOptions = list(httr::upload_file(options))
      )
    }
    if (!is.null(labels)) {
      body <- c(body,
        labels = list(jsonlite::toJSON(as.list(labels), auto_unbox = TRUE))
      )
    }

    crom_dat <-
      http_post(
        url = make_url(url, "api/workflows/v1"),
        body = body,
        encode = "multipart",
        token = token
      )
    dplyr::as_tibble(crom_dat)
  }
