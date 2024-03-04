test_that("cromwell_outputs", {
  vcr::use_cassette("cromwell_outputs_prep", {
    res <- cromwell_submit_batch(wdl = file_hello, params = file_inputs)
  })

  # Sys.sleep(45) # Needed only for recording new fixture #nolint

  vcr::use_cassette("cromwell_outputs", {
    outputs <- suppressWarnings(cromwell_outputs(res$id))
  })

  expect_s3_class(outputs, "data.frame")
  expect_equal(outputs$workflow_id, res$id)
  expect_match(outputs$pathToOutput, res$id)
})
