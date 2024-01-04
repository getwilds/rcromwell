#' Abort a workflow job on Cromwell
#'
#' Aborts any given workflow job on Cromwell.
#'
#' @param workflow_id Unique workflow id of the job you wish to kill.
#' @return Returns the response from the API post
#' @author Amy Paguirigan
#' @inheritSection workflowOptions Important
#' @export
cromwellAbort <- function(workflow_id) {
  check_url()
  crom_mssg("Aborting job in Cromwell")
  cromAbort <-
    httpPOST(url =
      make_url("api/workflows/v1",
        workflow_id,
        "abort"
      )
    )
  cromResponse <-
    data.frame(cromAbort, stringsAsFactors = F)
  return(cromResponse)
}
