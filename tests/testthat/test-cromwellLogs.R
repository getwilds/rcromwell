test_that("cromwellLogs", {
  vcr::use_cassette("cromwellLogs_before", {
    res <- cromwellSubmitBatch(WDL=file_hello, Params=file_inputs)
    # Sys.sleep(10) # Needed only for recording new fixture
    logs_empty <- cromwellLogs(res$id)
  })

  # Sys.sleep(45) # Needed only for recording new fixture

  vcr::use_cassette("cromwellLogs_after", {
    logs_not_empty <- cromwellLogs(res$id)
  })

  expect_equal(NROW(logs_empty), 0)
  expect_gt(NROW(logs_not_empty), 0)
})
