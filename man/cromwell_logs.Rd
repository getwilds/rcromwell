% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cromwellLogs.R
\name{cromwell_logs}
\alias{cromwell_logs}
\title{Gets logs for a workflow in Cromwell}
\usage{
cromwell_logs(workflow_id, url = cw_url(), token = NULL)
}
\arguments{
\item{workflow_id}{(character) A single workflow ID}

\item{url}{(character) base url for your Cromwell server. optional. if not
supplied set the url as the env var \code{CROMWELLURL}}

\item{token}{(character) we do not recommend passing your token
here as a string. Either pass it using \code{\link[=Sys.getenv]{Sys.getenv()}} or save your
token as the env var \code{PROOF_TOKEN} and then passing nothing to this
param and we'll find it}
}
\value{
a tibble with a variable number of forws; with columns:
\itemize{
\item callName (chr)
\item stderr (chr)
\item stdout (chr)
\item attempt (chr)
\item shardIndex (chr)
\item workflow_id (chr)
}
}
\description{
Gets logs for a workflow in Cromwell
}
\section{Important}{

Requires valid Cromwell server URL to be set. See \link{cromwell_settings}
}

\author{
Amy Paguirigan, Scott Chamberlain
}
