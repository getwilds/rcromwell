test_that("cromwell_jobs", {
  vcr::use_cassette("cromwell_jobs",
    {
      res <- cromwell_jobs()
    },
    match_requests_on = c("method", "host", "path")
  )

  expect_s3_class(res, "tbl")
  expect_equal(NCOL(res), 8)
})
