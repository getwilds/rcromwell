library("vcr") # *Required* as vcr is set up on loading
invisible(vcr::vcr_configure(
  dir = vcr::vcr_test_path("fixtures")
))
vcr::check_cassette_names()

file_hello <- system.file("examples/hello.wdl", package = "fh.wdlR")
file_inputs <- system.file("examples/inputs.json", package = "fh.wdlR")
file_inputs_bad <- system.file("examples/inputs_bad.json", package = "fh.wdlR")
