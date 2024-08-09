library("vcr") # *Required* as vcr is set up on loading
invisible(vcr::vcr_configure(
  dir = vcr::vcr_test_path("fixtures")
))
vcr::check_cassette_names()

sys_file <- function(..., pkg = "rcromwell") {
  system.file(..., package = pkg)
}

file_hello <- sys_file("examples/hello.wdl")
file_inputs <- sys_file("examples/inputs.json")
file_inputs_bad <- sys_file("examples/inputs_bad.json")
file_workflow_opts <- sys_file("examples/workflow_options.json")

# suppress messages for the test run
cromwell_config(verbose = FALSE)

cromwell_localhost_up <- function() {
  try8000 <- tryCatch(
    curl::curl_fetch_memory("localhost:8000"),
    error = function(e) e
  )
  !inherits(try8000, "error")
}
