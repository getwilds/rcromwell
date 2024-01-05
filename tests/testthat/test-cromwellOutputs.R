test_that("cromwellOutputs", {
  vcr::use_cassette("cromwellOutputs_prep", {
    res <- cromwellSubmitBatch(WDL = file_hello, Params = file_inputs)
  })

  # Sys.sleep(45) # Needed only for recording new fixture

  vcr::use_cassette("cromwellOutputs", {
    outputs <- suppressWarnings(cromwellOutputs(res$id))
  })

  expect_s3_class(outputs, "data.frame")
  expect_equal(outputs$workflow_id, res$id)
  expect_match(outputs$pathToOutput, res$id)
})
