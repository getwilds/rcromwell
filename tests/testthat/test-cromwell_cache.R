test_that("cromwell_cache_http", {
  vcr::use_cassette("cromwell_cache_http_prep", {
    job <- cromwell_submit_batch(wdl = file_hello, params = file_inputs)
  })

  # Sys.sleep(30) # Needed only for recording new fixture #nolint

  vcr::use_cassette("cromwell_cache_http", {
    res <- cromwell_cache_http(job$id)
  })

  expect_type(res, "list")
  expect_length(res$calls, 1)

  # nolint start
  # data for cromwell_cache_process test block
  # http_result_calls <- res
  # save(http_result_calls, file = "tests/testthat/cache_http_output.RData")
  # http_result_no_calls <- jsonlite::fromJSON("{\"workflowName\":\"test\",\"workflowProcessingEvents\":[{\"cromwellId\":\"cromid-01b35cf\",\"description\":\"PickedUp\",\"timestamp\":\"2024-03-18T16:35:35.495Z\",\"cromwellVersion\":\"86\"}],\"actualWorkflowLanguageVersion\":\"draft-2\",\"submittedFiles\":{\"workflow\":\"task hello {\\n  String name\\n\\n  command {\\n    echo 'Hello ${name}!'\\n  }\\n  output {\\n    File response = stdout()\\n  }\\n}\\n\\nworkflow test {\\n  call hello\\n}\\n\\n\",\"root\":\"\",\"options\":\"{\\n\\n}\",\"inputs\":\"{\\\"test.hello.name\\\":\\\"World\\\"}\",\"workflowUrl\":\"\",\"labels\":\"{}\"},\"calls\":{},\"outputs\":{},\"workflowRoot\":\"/Users/schambe3/github/cromwell/cromwell-executions/test/92dd16fa-75ec-4d57-9d49-0e446c86506b\",\"actualWorkflowLanguage\":\"WDL\",\"status\":\"Running\",\"start\":\"2024-03-18T16:35:35.497Z\",\"id\":\"92dd16fa-75ec-4d57-9d49-0e446c86506b\",\"inputs\":{\"test.hello.name\":\"World\"},\"labels\":{\"cromwell-workflow-id\":\"cromwell-92dd16fa-75ec-4d57-9d49-0e446c86506b\"},\"submission\":\"2024-03-18T16:35:20.738Z\"}")
  # save(http_result_no_calls, file = "tests/testthat/cache_http_output_no_calls.RData")
  # nolint end
})

test_that("cromwell_cache_process", {
  # load("tests/testthat/cache_http_output.RData") #nolint
  # load("tests/testthat/cache_http_output_no_calls.RData") #nolint
  load("cache_http_output.RData")
  load("cache_http_output_no_calls.RData")
  workflow_id <- "83a9f3e5-7fe8-4a1e-92fd-83c97532bd2c"
  with_calls <- cromwell_cache_process(http_result_calls, workflow_id)
  without_calls <- cromwell_cache_process(http_result_no_calls, workflow_id)

  expect_s3_class(with_calls, "tbl")
  expect_s3_class(without_calls, "tbl")
  expect_gt(NROW(with_calls), 0)
  expect_equal(NROW(without_calls), 0)
})

test_that("cromwell_cache", {
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
