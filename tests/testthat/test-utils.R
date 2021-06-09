test_that("generate_subject_ids works", {
  expect_equal(
    generate_subject_ids(9, prefix = "SUBJ"),
    c("SUBJ1", "SUBJ2", "SUBJ3", "SUBJ4", "SUBJ5", "SUBJ6",
      "SUBJ7", "SUBJ8", "SUBJ9")
  )

  expect_equal(
    generate_subject_ids(1000, prefix = "S") %>%
      nchar() %>%
      unique(),
    5
  )
})
