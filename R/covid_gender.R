#' Extract COVID-19 gender breakdown data from source
#'
#' @description The \code{covid_*} family of functions pull COVID-19 tracking
#' data from OpenDataPhilly.org which provides .csv files taken from a CARTO SQL database API.
#'
#' \code{covid_gender} pulls data from the COVID Tests and Cases registry and presents the output in a pre-processed, tidy dataset.
#'
#' Note that this extraction provides only a sum total since the start of recording and not a summary over time.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr mutate select case_when rename transmute summarise group_by
#' @export
#' @references
#' COVID Tests and Cases: \url{https://www.opendataphilly.org/dataset/covid-cases}
#' COVID Deaths: \url{https://www.opendataphilly.org/dataset/covid-deaths}

covid_gender <- function(){

  covid_gender <- read_csv("https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+covid_cases_by_sex&filename=covid_cases_by_Gender&format=csv&skipfields=cartodb_id") %>%
    select(
      sex,
      count,
      etl_timestamp
    ) %>%
    rename(
      Gender = sex,
      Count = count,
      TimeStamp = etl_timestamp
    ) %>%
    transmute(
      Gender = case_when(
        Gender == "UNKNOWN"~ "Unknown",
        TRUE ~ Gender),
      Count,
      Outcome = "Positive",
      TimeStamp
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
    ) %>%
    group_by(Gender) %>%
    summarise(Count = sum(Count)) %>%
    mutate(Outcome = "Died",
           TimeStamp = unique(covid_gender$TimeStamp))

  covid_gender <- rbind(covid_gender, covid_death_age)
  covid_gender <- covid_gender[,c(1,3,2,4)] # Reorder to display properly

}
