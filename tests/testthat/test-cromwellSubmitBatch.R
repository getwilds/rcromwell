test_that("cromwellSubmitBatch", {
  hello <- system.file("examples/hello.wdl", package = "fh.wdlR")
  inputs <- system.file("examples/inputs.json", package = "fh.wdlR")

  vcr::use_cassette("cromwellSubmitBatch", {
    res <- cromwellSubmitBatch(WDL=hello, Params=inputs)
  })

  expect_equal(NROW(res), 1)
  expect_named(res, c("id", "status"))
  expect_equal(res$status, "Submitted")
})
