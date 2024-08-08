test_that("cromwell_call", {
  vcr::use_cassette("cromwell_call_prep", {
    res <- cromwell_submit_batch(wdl = file_hello, params = file_inputs)
  })

  # Sys.sleep(30) # Needed only for recording new fixture #nolint

  vcr::use_cassette("cromwell_call", {
    res <- cromwell_call(res$id)
  })

  expect_s3_class(res, "data.frame")
  expect_s3_class(res, "tbl")
  expect_equal(res$callName, "hello")
})
