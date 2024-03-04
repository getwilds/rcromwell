test_that("cromwell_logs", {
  vcr::use_cassette("cromwell_logs_before", {
    res <- cromwell_submit_batch(wdl = file_hello, params = file_inputs)
    # Sys.sleep(10) # Needed only for recording new fixture #nolint
    logs_empty <- cromwell_logs(res$id)
  })

  # Sys.sleep(45) # Needed only for recording new fixture #nolint

  vcr::use_cassette("cromwell_logs_after", {
    logs_not_empty <- cromwell_logs(res$id)
  })

  expect_equal(NROW(logs_empty), 0)
  expect_gt(NROW(logs_not_empty), 0)
})
