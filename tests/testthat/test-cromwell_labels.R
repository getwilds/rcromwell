test_that("cromwell_labels", {
  my_labels <- data.frame(
    "workflowType" = "AppSubmission",
    "Label" = "Apple",
    "secondaryLabel" = "Orange"
  )

  vcr::use_cassette("cromwell_labels_submit", {
    res <- cromwell_submit_batch(
      wdl = file_hello, params = file_inputs,
      labels = my_labels
    )
  })

  # Sys.sleep(20) # Needed only for recording new fixture #nolint

  vcr::use_cassette("cromwell_labels_data", {
    labels_from_workflow <- cromwell_labels(res$id)
  })

  expect_length(labels_from_workflow, 4)
  expect_equal(labels_from_workflow$Label, "Apple")
  expect_equal(labels_from_workflow$secondaryLabel, "Orange")
})
