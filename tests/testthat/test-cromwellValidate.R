test_that("cromwellValidate", {
  vcr::use_cassette("cromwellValidate", {
  	path <- system.file("examples/hello.wdl", package = "fh.wdlR")
    res <- cromwellValidate(path)
  })

  expect_type(res, "list")
  expect_gt(length(res), 10)
})
