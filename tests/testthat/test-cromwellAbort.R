test_that("cromwellAbort", {
  vcr::use_cassette("cromwellAbort", {
    job <- cromwellSubmitBatch(wdl = file_hello, params = file_inputs)

    # Sys.sleep(20) # Needed only for recording new fixture

    res <- cromwellAbort(job$id)
  })

  expect_equal(NROW(res), 1)
  expect_named(res, c("id", "status"))
  expect_equal(res$status, "Aborting")
})
