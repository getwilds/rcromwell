test_that("cromwell_jobs", {
  vcr::use_cassette("cromwell_jobs",
    {
      res <- cromwell_jobs()
    },
    match_requests_on = c("method", "host", "path")
  )

  expect_s3_class(res, "data.frame")
  expect_s3_class(res, "tbl")
  expect_equal(NCOL(res), 8)
  # explicit column ordering for better understanding
  expect_equal(names(res)[1], "workflow_name")
})
