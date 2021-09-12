test_that("covid_outcome works", {
  covid_outcome <- covid_outcome()

  class_list <- purrr::map(.x = covid_outcome, .f = class)

  expect_equal(ncol(covid_outcome), 5)

  expect_equal(colnames(covid_outcome),
               c("TestResult",
                 "Outcome",
                 "Count",
                 "TimeStamp",
                 "Status" ))
  expect_true(class_list$TestResult == "character")
  expect_true(class_list$Outcome == "character")
  expect_true(class_list$Count == "numeric")
  expect_true("POSIXct" %in% class_list$TimeStamp)
  expect_true("POSIXt" %in% class_list$TimeStamp)
  expect_true(class_list$Status == "character")
})
