test_that("covid_gender works", {
  covid_gender <- covid_gender()

  class_list <- purrr::map(.x = covid_gender(), .f = class)

  expect_equal(ncol(covid_gender), 4)

  expect_equal(colnames(covid_gender),
               c("Gender",
                 "Outcome",
                 "Count",
                 "TimeStamp"))
  expect_true(class_list$Gender == "character")
  expect_true(class_list$Outcome == "character")
  expect_true(class_list$Count == "numeric")
  expect_true("POSIXct" %in% class_list$TimeStamp)
  expect_true("POSIXt" %in% class_list$TimeStamp)
})
