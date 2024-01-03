test_that("cromwellWorkflow", {
  vcr::use_cassette("cromwellWorkflow_prep", {
    job <- cromwellSubmitBatch(WDL=file_hello, Params=file_inputs)
  })

  # Sys.sleep(30) # Needed only for recording new fixture

  vcr::use_cassette("cromwellWorkflow", {
    res <- cromwellWorkflow(job$id)
  })

  expect_s3_class(res, "data.frame")
  expect_equal(res$workflow_id, job$id)
})
