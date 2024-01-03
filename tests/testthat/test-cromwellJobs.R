test_that("cromwellJobs", {
  vcr::use_cassette("cromwellJobs", {
    res <- cromwellJobs()
  })

  expect_s3_class(res, "tbl")
  expect_equal(NCOL(res), 8)
})
