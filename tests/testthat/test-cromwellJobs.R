test_that("cromwellJobs", {
  vcr::use_cassette("cromwellJobs", {
    res <- cromwellJobs()
  }, match_requests_on = c("method", "host", "path"))

  expect_s3_class(res, "tbl")
  expect_equal(NCOL(res), 8)
})
