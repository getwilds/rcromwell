#' Get AWS job logs
#'
#'
#' @param jobId JobId from AWS of a job to request logs for from a Crowmell workflow
#' @return A list of two entries: one the AWS Cloudwatch Logs (log), and one the AWS Batch job metadata (Job) for the jobId submitted.
#' @author Amy Paguirigan
#' @details
#' Requires valid Cromwell URL to be set in the environment and valid AWS credentials as well.
#' @examples
#' TBD
#' @export
getAWSLogs <-
  function(jobId=NULL) {
    if("" %in% c(Sys.getenv("S3A"), Sys.getenv("S3SA"))) {
      stop("AWS credentials are not set.")
    } else if (is.null(jobId) == T) {
      stop("Plesae provide a valid jobId to query.")
      } else {
        # Set AWS creds with required env var names
      Sys.setenv("AWS_ACCESS_KEY_ID" = Sys.getenv("S3A"),
                 "AWS_SECRET_ACCESS_KEY" = Sys.getenv("S3SA"),
                 "AWS_DEFAULT_REGION" = "us-west-2")
        # define which AWS services this function will use
        batchsvc <- paws::batch()
        watchman <- paws::cloudwatchlogs()
        }

    jobatAWS <- batchsvc$describe_jobs(jobId)
    justdat <- jobatAWS$jobs[[1]]
    noattempts <- justdat %>% purrr::discard(names(.) == "attempts") %>%
      purrr::simplify_all() %>% purrr::compact()
    logStream <- noattempts[["container"]][["logStreamName"]]
    log <- watchman$get_log_events(logGroupName = "/aws/batch/job",
                                    logStreamName = logStream)
    tableLog <- purrr::map_dfr(log$events, as.data.frame)
    return(list(log = tableLog, job = noattempts))
  }
