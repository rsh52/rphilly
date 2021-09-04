#' Extract COVID-19 age data from source
#'
#' @description The \code{covid_*} family of functions pull COVID-19 tracking
#' data from OpenDataPhilly.org which provides .csv files taken from a CARTO SQL database API.
#'
#' \code{covid_age} pulls positive COVID-19 aggregate age breakdown data from the COVID Tests and Cases registry as well as the COVID Deaths registry and presents the output in a pre-processed, tidy dataset.
#'
#' Note that this extraction provides only aggregate summation of data from the start of recording to the ETL \code{TimeStamp}.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr mutate select case_when rename
#' @export
#' @references
#' COVID Tests and Cases: \url{https://www.opendataphilly.org/dataset/covid-cases}
#' COVID Deaths: \url{https://www.opendataphilly.org/dataset/covid-deaths}

covid_age <- function(){

  covid_age <- read_csv("https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+covid_cases_by_age&filename=covid_cases_by_age&format=csv&skipfields=cartodb_id") %>%
    select(
      age,
      count,
      etl_timestamp
    ) %>%
    rename(
      Age = age,
      Count = count,
      TimeStamp = etl_timestamp
    ) %>%
    mutate(
      Gender = "All",
      Outcome = "Positive"
    )

  covid_death_age <- read_csv("https://phl.carto.com/api/v2/sql?filename=covid_deaths_by_age&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20FROM%20covid_deaths_by_age") %>%
    select(
      age,
      count,
      etl_timestamp,
      gender
    ) %>%
    rename(
      Gender = gender,
      Age = age,
      Count = count,
      TimeStamp = etl_timestamp
    ) %>%
    mutate(
      Outcome = "Died"
    )

  rbind(covid_age, covid_death_age)

}
