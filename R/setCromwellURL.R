#' Set CROMWELLURL variable based on a gizmo SLURM jobId
#'
#'
#' @param FredHutchId Your Fred Hutch username
#' @param jobId The SLURM job id for your Cromwell server
#' @param port The port you specified in your Cromwell config file (`fh-slurm-cromwell.config`), default is "2020" from the template here: https://github.com/FredHutch/diy-cromwell-server.
#' @return Sets your environment variable CROMWELLURL to be that of the Cromwell server for the job specified.
#' @author Amy Paguirigan
#' @details
#' Will require your Fred Hutch Id and will prompt you to enter your Fred Hutch password.
#' @export
setCromwellURL <- function(FredHutchId = NULL, jobId = NULL, port = "2020") {
  if (is.null(FredHutchId) == T) {
    stop("Please supply your Fred Hutch id.")
  }
  if (is.null(jobId) == T) {
    stop("Please supply the SLURM jobId of your Cromwell server job.")
  }

  # Make an ssh session to rhino and it will prompt for password
  session <- ssh::ssh_connect(paste0(FredHutchId, "@rhino"))
  getNode <- ssh::ssh_exec_internal(session, command = paste0('squeue -o "%R" -j ', jobId))
  nodeName <- sub("^.*)", "", gsub("\n", "", rawToChar(getNode$stdout)))
  if (nodeName == ""){
    message("Re-querying for node name.")
    Sys.sleep(2)
    getNode <- ssh::ssh_exec_internal(session, command = paste0('squeue -o "%R" -j ', jobId))
    nodeName <- sub("^.*)", "", gsub("\n", "", rawToChar(getNode$stdout)))
    if (nodeName == ""){
      ssh::ssh_disconnect(session)
      stop("I don't know what node your job is on, but use `squeue -u <username>` on `rhino` to find out. Note, it may be that your job has not been assigned resources yet.")
    }
  }
  message(paste0("Your Cromwell server is on node: ", nodeName))
  message(paste0("To use the Swagger UI in a Browser, go to: http://", nodeName, ":", port))

  Sys.setenv(CROMWELLURL=paste0("http://", nodeName, ":", port))
  message(paste0("CROMWELLURL is currently set to: ", Sys.getenv("CROMWELLURL")))

  ssh::ssh_disconnect(session)
  return(list(node = nodeName, jobId = jobId))
}
