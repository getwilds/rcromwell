#' Start a Cromwell server on Gizmo
#'
#'
#'
#' @param FredHutchId Your Fred Hutch username
#' @param port The port want to connect to for this server.
#' @param pathToServerLogs Full path in our file system to where you want your Cromwell server logs to be written.
#' @param pathToServerScript Full path in our file system to where you have saved the script used to start your Cromwell server (e.g. cromServer.sh, https://github.com/FredHutch/diy-cromwell-server).
#' @param pathToParams Full path in our file system to where you have saved the parameters you'd like your Cromwell server to use (e.g. cromwellParams.sh, https://github.com/FredHutch/diy-cromwell-server).
#' @param pathToConfig Full path in our file system to where you have saved the configuration file you'd like your Cromwell server to use (e.g. fh-slurm-cromwell.conf, https://github.com/FredHutch/diy-cromwell-server).
#' @param local Are you running this on your local machine (TRUE) or on the rhino's (FALSE)
#' @return Sets your environment variable CROMWELLURL to be that of the Cromwell server you just started and returns the information about the job ID and node you'll need.
#' @author Amy Paguirigan
#' @details
#' Will require your Fred Hutch Id and will prompt you to enter your Fred Hutch password.
#' Suggestions for parameters are:
#' pathToServerLogs = "/home/username/cromwell-home/cromwell-serverlogs/%A.txt"
#' pathToServerScript = "/home/username/cromwell-home/cromServer.sh"
#' pathToParams = "/home/username/cromwell-home/cromwellParams.sh"
#' port = "2020"
#' pathToConfig = "/home/username/cromwell-home/fh-slurm-cromwell.conf"
#' NOTE:  This has been discontinued in favor of setting up Cromwell in a more secure way.
cromwellCreate <- function(FredHutchId = NULL, port = "2020",
                           pathToServerLogs = NULL,
                           pathToServerScript = NULL,
                           pathToParams = NULL,
                           pathToConfig = NULL,
                           local = TRUE) {
  if (is.null(FredHutchId) == T) {
    stop("Please supply your Fred Hutch id.")
  }
  if (is.null(pathToServerLogs) == T) {
    stop("Please supply the full path to where you want server logs to be saved.")
  }
  if (is.null(pathToServerScript) == T) {
    stop("Please supply the full path to where your Cromwell server script is saved (e.g., cromServer.sh).")
  }
  if (is.null(pathToParams) == T) {
    stop("Please supply the full path to where your Cromwell parameters file is saved (e.g., cromwellParams.sh).")
  }
  if (is.null(pathToConfig) == T) {
    stop("Please supply the full path to where the configuration file is saved (e.g., fh-slurm-cromwell.conf).")
  }


  rhino <- "@rhino03" # remove once tested

  if (local == T){
  # Make an ssh session to rhino and it will prompt for password
  session <- ssh::ssh_connect(paste0(FredHutchId, rhino))
  # send the command to gizmo to start your server and save the response
  setupServer <- ssh::ssh_exec_internal(session,
                                   command = paste("sbatch", "-o",
                                                   pathToServerLogs,
                                                   pathToServerScript,
                                                   pathToParams,
                                                   port,
                                                   pathToConfig,
                                                   sep = " "))
  message(gsub("\n", "", rawToChar(setupServer$stdout)))

  slurmJob <- sub("\\D*\n", "", gsub("Submitted batch job ", "", rawToChar(setupServer$stdout)))
  if(slurmJob == ""){
    ssh::ssh_disconnect(session)
    stop("Slurm Job ID is unset.")
  }
  nodecommand = paste0('squeue -o "%R" -j ', slurmJob)
  Sys.sleep(2)
  getNode <- ssh::ssh_exec_internal(session, command = nodecommand)
  nodeName <- sub("^.*)", "", gsub("\n", "", rawToChar(getNode$stdout)))
  if (nodeName %in% c("", " ")){
    message("Re-querying for node name.")
    Sys.sleep(2)
    getNode <- ssh::ssh_exec_internal(session, command = nodecommand)
    nodeName <- sub("^.*)", "", gsub("\n", "", rawToChar(getNode$stdout)))
    if (nodeName == ""){
      ssh::ssh_disconnect(session)
      stop("Your job likely has not yet been assigned a node yet.  Please use
      `squeue -u <username>` on `rhino` to find out if your job has been
           assigned or use the function `setCromwellURL()` in this package.")
    }
  }
  ssh::ssh_disconnect(session)
  }
  if (local == F) {
    setupServer <- system(command = paste("sbatch", "-o",
                                          pathToServerLogs,
                                          pathToServerScript,
                                          pathToParams,
                                          port,
                                          pathToConfig,
                                          sep = " "),
                          intern = TRUE)
    message(setupServer)
    slurmJob <- sub("\\D*\n", "", gsub("Submitted batch job ", "", setupServer))
    if(slurmJob == ""){
      stop("Slurm Job ID is unset.")
    }
    nodecommand = paste0('squeue -o "%R" -j ', slurmJob)
    Sys.sleep(2)
    getNode <- system(command = nodecommand, intern = TRUE)
    nodeName <- getNode[3]
    if (nodeName %in% c("", " ")){
      message("Re-querying for node name.")
      Sys.sleep(2)
      getNode <- system(command = nodecommand, intern = TRUE)
      nodeName <- getNode[3]
      if (nodeName == ""){
        stop("Your job likely has not yet been assigned a node yet.  Please use
             `squeue -u <username>` on `rhino` to find out if your job has been
             assigned or use the function `setCromwellURL()` in this package.")
      }
    }
  }

  message(paste0("Your Cromwell server is on node: ", nodeName))
  message(paste0("To use the Swagger UI in a Browser, go to: http://", nodeName, ":", port))

  Sys.setenv(CROMWELLURL=paste0("http://", nodeName, ":", port))
  message(paste0("Env variable CROMWELLURL is currently set to: ", Sys.getenv("CROMWELLURL")))


  return(list(node = nodeName, jobId = slurmJob))
}
