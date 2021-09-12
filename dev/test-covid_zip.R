test_that("covid_zip works", {
  covid_zip <- covid_zip()

  class_list <- purrr::map(.x = covid_zip, .f = class)

  expect_equal(ncol(covid_zip), 4)

  expect_equal(colnames(covid_zip),
               c("Outcome",
                 "ZIP",
                 "Count",
                 "TimeStamp"))

  expect_true(class_list$Outcome == "character")
  expect_true(class_list$ZIP == "numeric")
  expect_true(class_list$Count == "numeric")
  expect_true("POSIXct" %in% class_list$TimeStamp)
  expect_true("POSIXt" %in% class_list$TimeStamp)
})
