test_that("cromwell_cache`", {
  vcr::use_cassette("cromwell_cache_prep", {
    job <- cromwell_submit_batch(wdl = file_hello, params = file_inputs)
  })

  # Sys.sleep(30) # Needed only for recording new fixture #nolint

  vcr::use_cassette("cromwell_cache", {
    res <- cromwell_cache(job$id)
  })

  expect_s3_class(res, "data.frame")
  expect_equal(res$workflow_id, job$id)
})
