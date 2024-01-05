test_that("cromwellGlob", {
  vcr::use_cassette("cromwellGlob", {
    res <- cromwellSubmitBatch(WDL = file_hello, Params = file_inputs)
    # Sys.sleep(20) # Needed only for recording new fixture
    res <- cromwellGlob(res$id)
  })

  expect_type(res, "list")
  expect_gt(length(res), 10)
  expect_equal(res$status, "Running")
})
