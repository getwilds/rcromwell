test_that("cromwell_workflow", {
  vcr::use_cassette("cromwell_workflow_prep", {
    job <- cromwell_submit_batch(wdl = file_hello, params = file_inputs)
  })

  # Sys.sleep(30) # Needed only for recording new fixture #nolint

  vcr::use_cassette("cromwell_workflow", {
    res <- cromwell_workflow(job$id)
  })

  expect_s3_class(res, "data.frame")
  expect_equal(res$workflow_id, job$id)

  ## processing function
  vcr::use_cassette("cromwell_workflow_http", {
    meta <- cromwell_workflow_http(job$id, cw_url(), NULL)
  })

  out <- cromwell_workflow_process(meta, job$id)
  expect_s3_class(out, "data.frame")
  expect_equal(out$workflow_id, job$id)
})
