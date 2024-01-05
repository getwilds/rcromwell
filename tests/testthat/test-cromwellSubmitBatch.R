test_that("cromwellSubmitBatch", {
  vcr::use_cassette("cromwellSubmitBatch", {
    res <- cromwellSubmitBatch(wdl = file_hello, params = file_inputs)
  })

  expect_equal(NROW(res), 1)
  expect_named(res, c("id", "status"))
  expect_equal(res$status, "Submitted")
})
