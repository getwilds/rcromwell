test_that("cromwell_submit_batch", {
  vcr::use_cassette("cromwell_submit_batch", {
    res <- cromwell_submit_batch(wdl = file_hello, params = file_inputs)
  })

  expect_equal(NROW(res), 1)
  expect_named(res, c("id", "status"))
  expect_equal(res$status, "Submitted")
})

# addresses: https://github.com/getwilds/rcromwell/issues/32
test_that("cromwell_submit_batch_workflow_options", {
  vcr::use_cassette("cromwell_submit_batch_workflow_options", {
    res <- cromwell_submit_batch(
      wdl = file_hello,
      params = file_inputs,
      options = file_workflow_opts
    )
  })

  expect_equal(NROW(res), 1)
  expect_named(res, c("id", "status"))
  expect_equal(res$status, "Submitted")
})
