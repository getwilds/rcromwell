pkg_env <- new.env() # nocov start

.onLoad <- function(libname, pkgname) {
  # use the same timezone throughout the package
  pkg_env$tzone <<- "US/Pacific"
  pkg_env$verbose <<- TRUE
} # nocov end
