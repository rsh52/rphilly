test_that("covid_race works", {
  covid_race <- covid_race()

  class_list <- purrr::map(.x = covid_race, .f = class)

  expect_equal(ncol(covid_race), 4)

  expect_equal(colnames(covid_race),
               c("Race",
                 "Count",
                 "TimeStamp",
                 "Percentage" ))
  expect_true(class_list$Race == "character")
  expect_true(class_list$Count == "numeric")
  expect_true("POSIXct" %in% class_list$TimeStamp)
  expect_true("POSIXt" %in% class_list$TimeStamp)
  expect_true(class_list$Percentage == "numeric")
})

test_that("covid_race_died_died works", {
  covid_race_died <- covid_race_died()

  class_list <- purrr::map(.x = covid_race_died, .f = class)

  expect_equal(ncol(covid_race_died), 4)

  expect_equal(colnames(covid_race_died),
               c("Race",
                 "Count",
                 "TimeStamp",
                 "Percentage" ))
  expect_true(class_list$Race == "character")
  expect_true(class_list$Count == "numeric")
  expect_true("POSIXct" %in% class_list$TimeStamp)
  expect_true("POSIXt" %in% class_list$TimeStamp)
  expect_true(class_list$Percentage == "numeric")
})
