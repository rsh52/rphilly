test_that("covid_date works", {

  covid_date <- covid_date()

  class_list <- purrr::map(.x = covid_date(), .f = class)

  expect_equal(ncol(covid_date), 4)

  expect_equal(colnames(covid_date),
               c("ResultDate",
                 "Count",
                 "Outcome",
                 "TimeStamp"))
  expect_true(class_list$ResultDate == "Date")
  expect_true(class_list$Count == "numeric")
  expect_true("POSIXct" %in% class_list$TimeStamp)
  expect_true("POSIXt" %in% class_list$TimeStamp)
  expect_true(class_list$Outcome == "character")

})
