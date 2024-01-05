test_that("cromwellCache`", {
  vcr::use_cassette("cromwellCache_prep", {
    job <- cromwellSubmitBatch(wdl = file_hello, params = file_inputs)
  })

  # Sys.sleep(30) # Needed only for recording new fixture

  vcr::use_cassette("cromwellCache", {
    res <- cromwellCache(job$id)
  })

  expect_s3_class(res, "data.frame")
  expect_equal(res$workflow_id, job$id)
})
