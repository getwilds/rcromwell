test_that("cromwell_validate", {
  vcr::use_cassette("cromwell_validate", {
    res <- cromwell_validate(file_hello)
  })

  expect_type(res, "list")
  expect_gt(length(res), 10)
})
