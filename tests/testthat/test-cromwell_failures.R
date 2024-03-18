# FIXME: cromwell_failures doesn't appear to be working. At least for the
# case where data is in the $failures slot at the top level of the list
# the parsing code doesn't grab it - perhaps the API response has changed

test_that("cromwell_failures", {
  vcr::use_cassette("cromwell_failures_prep", {
    job <- cromwell_submit_batch(wdl = file_hello, params = file_inputs_bad)
  })

  # Sys.sleep(30) # Needed only for recording new fixture #nolint

  vcr::use_cassette("cromwell_failures", {
    res <- cromwell_failures(job$id)
  })

  expect_s3_class(res, "data.frame")
  expect_equal(NROW(res), 0)
})
