test_that("cromwellLogs", {
  hello <- system.file("examples/hello.wdl", package = "fh.wdlR")
  inputs <- system.file("examples/inputs.json", package = "fh.wdlR")

  vcr::use_cassette("cromwellLogs_before", {
    res <- cromwellSubmitBatch(WDL=hello, Params=inputs)
    logs_empty <- cromwellLogs(res$id)
  })

  # Sys.sleep(45) # Needed only for recording new fixture

  vcr::use_cassette("cromwellLogs_after", {
    logs_not_empty <- cromwellLogs(res$id)
  })

  expect_equal(NROW(logs_empty), 0)
  expect_gt(NROW(logs_not_empty), 0)
})
