test_that("covid_vaccinations works", {
  covid_vaccinations <- covid_vaccinations()

  # Test overall structure -----------------------------------------------------
  expect_equal(length(covid_vaccinations), 6)
  expect_true(is.list(covid_vaccinations))
  expect_equal(class(covid_vaccinations), "list")

  # Test age structure ---------------------------------------------------------
  age <- data.frame(
    age = "character",
    partially_vaccinated = "double",
    fully_vaccinated = "double",
    etl_timestamp = "double"
  )
  expect_true(
    all(
      sapply(covid_vaccinations$age, typeof) == age
    ) == TRUE
  )

  # Test census_tract structur -------------------------------------------------
  census_tract <- data.frame(
    objectid = "double",
    census_tract_geoid = "double",
    dose1_percent_range = "character",
    etl_timestamp = "double"
  )
  expect_true(
    all(
      sapply(covid_vaccinations$census_tract, typeof) == census_tract
    ) == TRUE
  )

  # Test race structure --------------------------------------------------------
  race <- data.frame(
    race = "character",
    partially_vaccinated = "double",
    fully_vaccinated = "double",
    etl_timestamp = "double"
  )
  expect_true(
    all(
      sapply(covid_vaccinations$race, typeof) == race
    ) == TRUE
  )

  # Test sex structure ---------------------------------------------------------
  sex <- data.frame(
    sex = "character",
    partially_vaccinated = "double",
    fully_vaccinated = "double",
    etl_timestamp = "double"
  )
  expect_true(
    all(
      sapply(covid_vaccinations$sex, typeof) == sex
    ) == TRUE
  )

  # Test total structure ---------------------------------------------------------
  total <- data.frame(
    partially_vaccinated = "double",
    fully_vaccinated = "double",
    etl_timestamp = "double"
  )
  expect_true(
    all(
      sapply(covid_vaccinations$total, typeof) == total
    ) == TRUE
  )

  # Test zip structure ---------------------------------------------------------
  zip <- data.frame(
    zip_code = "double",
    partially_vaccinated = "double",
    fully_vaccinated = "double",
    etl_timestamp = "double"
  )
  expect_true(
    all(
      sapply(covid_vaccinations$zip, typeof) == zip
    ) == TRUE
  )


})
