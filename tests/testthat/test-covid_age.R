test_that("covid_age works", {
  covid_age <- covid_age()

  class_list <- purrr::map(.x = covid_age(), .f = class)

  expect_equal(ncol(covid_age), 5)

  expect_equal(colnames(covid_age),
               c("Age",
                 "Count",
                 "TimeStamp",
                 "Gender",
                 "Outcome"))
  expect_true(class_list$Age == "character")
  expect_true(class_list$Count == "numeric")
  expect_true("POSIXct" %in% class_list$TimeStamp)
  expect_true("POSIXt" %in% class_list$TimeStamp)
  expect_true(class_list$Gender == "character")
  expect_true(class_list$Outcome == "character")
})
