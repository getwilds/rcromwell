test_that("cromwellValidate", {
  vcr::use_cassette("cromwellValidate", {
    res <- cromwellValidate(file_hello)
  })

  expect_type(res, "list")
  expect_gt(length(res), 10)
})
