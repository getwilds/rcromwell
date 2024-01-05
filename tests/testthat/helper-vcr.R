library("vcr") # *Required* as vcr is set up on loading
invisible(vcr::vcr_configure(
  dir = vcr::vcr_test_path("fixtures")
))
vcr::check_cassette_names()

file_hello <- system.file("examples/hello.wdl", package = "rcromwell")
file_inputs <- system.file("examples/inputs.json", package = "rcromwell")
file_inputs_bad <- system.file("examples/inputs_bad.json", package = "rcromwell")

# suppress messages for the test run
cromwellConfig(verbose = FALSE)
