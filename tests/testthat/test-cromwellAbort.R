test_that("cromwellAbort", {
  hello <- system.file("examples/hello.wdl", package = "fh.wdlR")
  inputs <- system.file("examples/inputs.json", package = "fh.wdlR")

  vcr::use_cassette("cromwellAbort", {
    job <- cromwellSubmitBatch(WDL=hello, Params=inputs)

    # Sys.sleep(20) # Needed only for recording new fixture

    res <- cromwellAbort(job$id)
  })

  expect_equal(NROW(res), 1)
  expect_named(res, c("id", "status"))
  expect_equal(res$status, "Aborting")
})
