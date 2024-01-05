# FIXME: cromwellFailures doesn't appear to be working. At least for the
# case where data is in the $failures slot at the top level of the list
# the parsing code doesn't grab it - perhaps the API response has changed

test_that("cromwellFailures", {
  vcr::use_cassette("cromwellFailures_prep", {
    job <- cromwellSubmitBatch(wdl = file_hello, params = file_inputs_bad)
  })

  # Sys.sleep(30) # Needed only for recording new fixture

  vcr::use_cassette("cromwellFailures", {
    res <- cromwellFailures(job$id)
  })

  expect_s3_class(res, "data.frame")
  expect_match(res$workflow_id, "No failure metadata available")
})
