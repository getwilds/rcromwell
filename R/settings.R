pkg_env <- new.env()

#' Cromwell Settings
#'
#' @name cromwellSettings
#'
#' @section Cromwell URL:
#' A URL for the Cromwell server you're interacting with is required.
#' The only way to do so is by setting the env var:
#'
#' `CROMWELLURL`
#'
#' You can set this env var in many different ways. From within R you can
#' do this with [cromwellConfig()] like:
#'
#' `cromwellConfig(url = "your/url")`
#'
#' Which is equivalent to
#'
#' `Sys.setenv(CROMWELLURL = "your/url")`
#'
#' Note that this only sets the url for the current R session and does
#' not persist among R sessions.
#'
#' Other options include:
#'
#' - Set `CROMWELLURL` env var in a `.bash_profile` file or equivalent
#' that will be picked up by R when it starts.
#' - Set `CROMWELLURL` env var in an IDE such as RStudio.
#' - Set `CROMWELLURL` env var by prefixing `R` on the command line, e.g.,
#' `CROMWELLURL=http://your/url R`
#'
#' @section Verbose messaging:
#'
#' You can set your preference for whether you get messages informing you
#' of various tasks `rcromwell` is performing. See [cromwellConfig()]
NULL

#' Cromwell Configuration
#'
#' @export
#' @param cromwell_url (character) Cromwell server URL. The full url,
#' for example "http://localhost:8000" or "https://mycromwell.com"
#' @param verbose (logical) verbose messaging. default: `TRUE`. this
#' toggles on (`TRUE`) or off (`FALSE`) all messages throughout the package.
#' @details Note that although the default value for `cromwell_url` is
#' `NULL`, we fetch the current value of `CROMWELLURL` env var internally
#' if `cromwell_url=NULL` - thus, we retain a value for `CROMWELLURL` if
#' it is set even if this function is run without passing anything to
#' `cromwell_url`
#' @examples \dontrun{
#' cromwellConfig()
#' cromwellConfig("https://mycromwellinstance.com")
#' cromwellConfig(verbose=FALSE)
#' }
cromwellConfig <- function(cromwell_url = NULL, verbose = TRUE) {
	if (is.null(cromwell_url)) cromwell_url <- Sys.getenv("CROMWELLURL")
	rlang::is_character(cromwell_url)
	Sys.setenv(CROMWELLURL = cromwell_url)

	rlang::is_logical(verbose)
	pkg_env$verbose <- verbose

	list(rcromwell_settings =
		list(url = cromwell_url, verbose = pkg_env$verbose))
}

crom_mssg <- function(...) {
	if (pkg_env$verbose) message(...)
}
