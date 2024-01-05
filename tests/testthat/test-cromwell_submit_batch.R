test_that("cromwell_submit_batch", {
  vcr::use_cassette("cromwell_submit_batch", {
    res <- cromwell_submit_batch(wdl = file_hello, params = file_inputs)
  })

  expect_equal(NROW(res), 1)
  expect_named(res, c("id", "status"))
  expect_equal(res$status, "Submitted")
})
