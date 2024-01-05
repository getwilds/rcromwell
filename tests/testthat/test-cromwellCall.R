test_that("cromwellCall", {
  vcr::use_cassette("cromwellCall_prep", {
    res <- cromwellSubmitBatch(wdl = file_hello, params = file_inputs)
  })

  # Sys.sleep(30) # Needed only for recording new fixture

  vcr::use_cassette("cromwellCall", {
    res <- cromwellCall(res$id)
  })

  expect_s3_class(res, "data.frame")
  expect_equal(res$callName, "hello")
})
