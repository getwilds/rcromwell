#' Start a Cromwell server on Gizmo
#'
#'
#'
#' @param FredHutchId Your Fred Hutch username
#' @param port The port you specified in your Cromwell config file (`fh-slurm-cromwell.config`), default is "2020" from the template here: https://github.com/FredHutch/diy-cromwell-server.
#' @param pathToServerLogs Full path in our file system to where you want your Cromwell server logs to be written.
#' @param pathToScript Full path in our file system to where you have saved the script used to start your Cromwell server (e.g. cromServer.sh, https://github.com/FredHutch/diy-cromwell-server).
#' @param pathToParams Full path in our file system to where you have saved the parameters you'd like your Cromwell server to use (e.g. cromwellParams.sh, https://github.com/FredHutch/diy-cromwell-server).
#' @return Sets your environment variable CROMWELLURL to be that of the Cromwell server you just started and returns the information about the job ID and node you'll need.
#' @author Amy Paguirigan
#' @details
#' Will require your Fred Hutch Id and will prompt you to enter your Fred Hutch password.
#' Suggestions for parameters are:
#' pathToServerLogs = "/home/username/cromwell/cromwell-serverlogs/%A.txt"
#' pathToScript = "/home/username/cromwell/cromServer.sh"
#' pathToParams = "/home/username/cromwell/cromwellParams.sh"
#' @export
cromwellCreate <- function(FredHutchId = NULL, port = "2020", pathToServerLogs = NULL,
                           pathToScript = NULL, pathToParams = NULL) {
  if (is.null(FredHutchId) == T) {
    stop("Please supply your Fred Hutch id.")
  }
  if (is.null(pathToServerLogs) == T) {
    stop("Please supply the full path to where you want server logs to be saved.")
  }
  if (is.null(pathToScript) == T) {
    stop("Please supply the full path to where your Cromwell server script is saved (e.g., cromServer.sh).")
  }
  if (is.null(pathToParams) == T) {
    stop("Please supply  the full path to where your Cromwell parameters file is saved (e.g., cromwellParams.sh).")
  }

  # Make an ssh session to rhino and it will prompt for password
  session <- ssh::ssh_connect(paste0(FredHutchId, "@rhino"))
  # send the command to gizmo to start your server and save the response
  setupServer <- ssh::ssh_exec_internal(session,
                                   command = paste("sbatch", "-o", pathToServerLogs, pathToScript, pathToParams, sep = " "))
  message(gsub("\n", "", rawToChar(setupServer$stdout)))
  slurmJob <- sub("\n", "", gsub("Submitted batch job ", "", rawToChar(setupServer$stdout)))

  if(slurmJob == ""){
    ssh::ssh_disconnect(session)
    stop("Slurm Job ID is unset.")
  }
  Sys.sleep(2)
  getNode <- ssh::ssh_exec_internal(session, command = paste0('squeue -o "%R" -j ', slurmJob))
  nodeName <- sub("^.*)", "", gsub("\n", "", rawToChar(getNode$stdout)))
  if (nodeName == ""){
    message("Re-querying for node name.")
    Sys.sleep(2)
    getNode <- ssh::ssh_exec_internal(session, command = paste0('squeue -o "%R" -j ', slurmJob))
    nodeName <- sub("^.*)", "", gsub("\n", "", rawToChar(getNode$stdout)))
    if (nodeName == ""){
      ssh::ssh_disconnect(session)
      stop("I don't know what node your job is on, but use `squeue -u <username>` on `rhino` to find out or use the function `setCromwellURL()` in this package. Note, it may be that your job has not been assigned resources yet.")
    }
  }
  message(paste0("Your Cromwell server is on node: ", nodeName))
  message(paste0("To use the Swagger UI in a Browser, go to: http://", nodeName, ":", port))

  Sys.setenv(CROMWELLURL=paste0("http://", nodeName, ":", port))
  message(paste0("CROMWELLURL is currently set to: ", Sys.getenv("CROMWELLURL")))

  ssh::ssh_disconnect(session)
  return(list(node = nodeName, jobId = slurmJob))
}
