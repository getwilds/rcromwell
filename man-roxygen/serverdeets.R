#' @param url (character) base url for your Cromwell server. optional. if not
#' supplied set the url as the env var `CROMWELLURL`
#' @param token (character) we do not recommend passing your token
#' here as a string. Either pass it using [Sys.getenv()] or save your
#' token as the env var `PROOF_TOKEN` and then passing nothing to this
#' param and we'll find it
