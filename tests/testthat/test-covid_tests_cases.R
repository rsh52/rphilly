test_that("covid_tests_cases works", {
  covid_tests_cases <- covid_tests_cases()

  # Test overall structure -----------------------------------------------------
  expect_equal(length(covid_tests_cases), 6)
  expect_true(is.list(covid_tests_cases))
  expect_equal(class(covid_tests_cases), "list")

  # Test age structure ---------------------------------------------------------
  age <- data.frame(
    age = "character",
    count = "double",
    etl_timestamp = "double",
    sex = "character",
    outcome = "character"
  )
  expect_true(
    all(
      sapply(covid_tests_cases$age, typeof) == age
    ) == TRUE
  )

  # Test date structure --------------------------------------------------------
  date <- data.frame(
    result_date = "double",
    count = "double",
    outcome = "character",
    etl_timestamp = "double"
  )
  expect_true(
    all(
      sapply(covid_tests_cases$date, typeof) == date
    ) == TRUE
  )

  # Test outcome structure -----------------------------------------------------
  outcome <- data.frame(
    test_result = "character",
    outcome = "character",
    count = "double",
    etl_timestamp = "double",
    status = "character"
  )
  expect_true(
    all(
      sapply(covid_tests_cases$outcome, typeof) == outcome
    ) == TRUE
  )

  # Test race structure --------------------------------------------------------
  race <- data.frame(
    race = "character",
    count = "double",
    etl_timestamp = "double",
    percentage = "double",
    outcome = "character"
  )
  expect_true(
    all(
      sapply(covid_tests_cases$race, typeof) == race
    ) == TRUE
  )

  # Test sex structure ---------------------------------------------------------
  sex <- data.frame(
    sex = "character",
    count = "double",
    outcome = "character",
    etl_timestamp = "double"
  )
  expect_true(
    all(
      sapply(covid_tests_cases$sex, typeof) == sex
    ) == TRUE
  )

  # Test zip structure ---------------------------------------------------------
  zip <- data.frame(
    outcome = "character",
    zip_code = "double",
    count = "double",
    etl_timestamp = "double"
  )
  expect_true(
    all(
      sapply(covid_tests_cases$zip, typeof) == zip
    ) == TRUE
  )


})
