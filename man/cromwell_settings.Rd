% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/settings.R
\name{cromwell_settings}
\alias{cromwell_settings}
\title{Cromwell Settings}
\description{
Cromwell Settings
}
\section{Cromwell URL}{

A URL for the Cromwell server you're interacting with is required.
The only way to do so is by setting the env var:

\code{CROMWELLURL}

You can set this env var in many different ways. From within R you can
do this with \code{\link[=cromwell_config]{cromwell_config()}} like:

\code{cromwell_config(url = "your/url")}

Which is equivalent to

\code{Sys.setenv(CROMWELLURL = "your/url")}

Note that this only sets the url for the current R session and does
not persist among R sessions.

Other options include:
\itemize{
\item Set \code{CROMWELLURL} env var in a \code{.bash_profile} file or equivalent
that will be picked up by R when it starts.
\item Set \code{CROMWELLURL} env var in an IDE such as RStudio.
\item Set \code{CROMWELLURL} env var by prefixing \code{R} on the command line, e.g.,
\verb{CROMWELLURL=http://your/url R}
}
}

\section{Verbose messaging}{


You can set your preference for whether you get messages informing you
of various tasks \code{rcromwell} is performing. See \code{\link[=cromwell_config]{cromwell_config()}}
}

