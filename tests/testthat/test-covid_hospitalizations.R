test_that("covid_hospitalizations works", {
  covid_hospitalizations <- covid_hospitalizations()

  # Test overall structure -----------------------------------------------------
  expect_equal(length(covid_hospitalizations), 5)
  expect_true(is.list(covid_hospitalizations))
  expect_equal(class(covid_hospitalizations), "list")

# Test age structure ---------------------------------------------------------
age <- data.frame(
  age = "character",
  hospitalized = "character",
  count = "double",
  etl_timestamp = "double"
)
expect_true(
  all(
    sapply(covid_hospitalizations$age, typeof) == age
  ) == TRUE
)

# Test date structure --------------------------------------------------------
date <- data.frame(
  date = "double",
  hospitalized = "character",
  count = "double",
  etl_timestamp = "double"
)
expect_true(
  all(
    sapply(covid_hospitalizations$date, typeof) == date
  ) == TRUE
)

# Test race structure --------------------------------------------------------
race <- data.frame(
  race = "character",
  hospitalized = "character",
  count = "double",
  etl_timestamp = "double"
)
expect_true(
  all(
    sapply(covid_hospitalizations$race, typeof) == race
  ) == TRUE
)

# Test sex structure ---------------------------------------------------------
sex <- data.frame(
  sex = "character",
  hospitalized = "character",
  count = "double",
  etl_timestamp = "double"
)
expect_true(
  all(
    sapply(covid_hospitalizations$sex, typeof) == sex
  ) == TRUE
)

# Test zip structure ---------------------------------------------------------
zip <- data.frame(
  zip_code = "double",
  hospitalized = "character",
  count = "double",
  etl_timestamp = "double"
)
expect_true(
  all(
    sapply(covid_hospitalizations$zip, typeof) == zip
  ) == TRUE
)

})
