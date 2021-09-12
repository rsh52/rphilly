#' Extract COVID-19 date data from source
#'
#' @description The \code{covid_*} family of functions pull COVID-19 tracking
#' data from data from \url{https://www.opendataphilly.org} which provides .csv files taken from a CARTO SQL database API.
#'
#' \code{covid_date} pulls test result and date data from the COVID Tests and Cases registry as well as the COVID Deaths registry and presents the output in a pre-processed, tidy dataset.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr mutate select case_when rename
#' @export
#' @references
#' - COVID Tests and Cases: \url{https://www.opendataphilly.org/dataset/covid-cases}
#'
#' - COVID Tests by Date: \url{https://www.opendataphilly.org/dataset/covid-cases/resource/85fead1a-0ec3-4856-98a9-27ba4fa941ee}
#'
#' - COVID Deaths: \url{https://www.opendataphilly.org/dataset/covid-deaths}
#'
#' - COVID Deaths by Date: \url{https://www.opendataphilly.org/dataset/covid-deaths/resource/45760e22-e2de-456f-ad0f-6c491b0f2e42}


covid_date <- function(){

  covid_date <- read_csv("https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+covid_cases_by_date&filename=covid_cases_by_date&format=csv&skipfields=cartodb_id") %>%
    select(collection_date,
           count,
           test_result,
           etl_timestamp) %>%
    rename(ResultDate = collection_date,
           Count = count,
           Outcome = test_result,
           TimeStamp = etl_timestamp) %>%
    mutate(
      Outcome = case_when(
        Outcome == "positive" ~ "Positive",
        Outcome == "negative" ~ "Negative",
        TRUE ~ NA_character_
      )
    )

  covid_deathdate <- read_csv("https://phl.carto.com/api/v2/sql?filename=covid_deaths_by_date&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20FROM%20covid_deaths_by_date") %>%
    select(clinical_date_of_death,
           count,
           covid_outcome,
           etl_timestamp) %>%
    rename(ResultDate = clinical_date_of_death,
           Count = count,
           Outcome = covid_outcome,
           TimeStamp = etl_timestamp) %>%
    mutate(
      Outcome = case_when(
        Outcome == "DIED" ~ "Died",
        TRUE ~ NA_character_
      )
    )

  rbind(covid_date, covid_deathdate)

}
