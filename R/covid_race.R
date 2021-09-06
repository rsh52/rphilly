#' Extract COVID-19 race breakdown data from source
#'
#' @description The \code{covid_*} family of functions pull COVID-19 tracking
#' data from data from \url{https://www.opendataphilly.org} which provides .csv files taken from a CARTO SQL database API.
#'
#' \code{covid_race} pulls data from the COVID Tests and Cases registry and presents the output in a pre-processed, tidy dataset. This data does not include race breakdown for patients who died. That is handled using \code{covid_race_died}.
#'
#' Note that this extraction provides only aggregate summation of data from the start of recording to the ETL \code{TimeStamp} which generally takes place at 15:00:00 UTC each day.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr mutate select case_when rename group_by filter
#' @export
#' @references
#'
#' - COVID Tests and Cases: \url{https://www.opendataphilly.org/dataset/covid-cases}
#'
#' - COVID Cases by Race: \url{https://www.opendataphilly.org/dataset/covid-cases/resource/a83adfe0-4dac-4718-9996-dd06a9a67192}
#'
#' - COVID Deaths: \url{https://www.opendataphilly.org/dataset/covid-deaths}
#'
#' - COVID Death by Race" \url{https://www.opendataphilly.org/dataset/covid-deaths/resource/c053a703-d99c-4003-93ca-caa5db17369a}

covid_race <- function(){
  read_csv("https://phl.carto.com/api/v2/sql?filename=covid_cases_by_race&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20FROM%20covid_cases_by_race") %>%
    filter(!is.na(racial_identity)) %>%
    mutate(
      Percentage = round(count/sum(count),3)*100,
      racial_identity = case_when(
        racial_identity == "AMERICAN INDIAN" ~ "American Indian",
        racial_identity == "PACIFIC ISLANDER" ~ "Pacific Islander",
        racial_identity == "NATIVE AMERICAN" ~ "Native American",
        racial_identity == "BLACK" ~ "Black",
        racial_identity == "HISPANIC" ~ "Hispanic",
        racial_identity == "OTHER" ~ "Other",
        racial_identity == "UNKNOWN" ~ "Unknown",
        racial_identity == "DECLINE" ~ "Declined to Specify",
        TRUE ~ racial_identity
      )
    ) %>%
    rename(
      Race = racial_identity,
      Count = count,
      TimeStamp = etl_timestamp
    )
}

#' Extract COVID-19 race breakdown data from source
#'
#' @description The \code{covid_*} family of functions pull COVID-19 tracking
#' data from data from \url{https://www.opendataphilly.org} which provides .csv files taken from a CARTO SQL database API.
#'
#' \code{covid_race_died} pulls data from the COVID Tests and Cases registry and presents the output in a pre-processed, tidy dataset. This data only includes patients who died, for patients who survived use \code{covid_race}.
#'
#' Note that this extraction provides only aggregate summation of data from the start of recording to the ETL \code{TimeStamp} which generally takes place at 15:00:00 UTC each day.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr mutate select case_when rename group_by filter
#' @export
#' @references
#'
#' - COVID Tests and Cases: \url{https://www.opendataphilly.org/dataset/covid-cases}
#' - COVID Deaths: \url{https://www.opendataphilly.org/dataset/covid-deaths}

covid_race_died <- function(){

  read_csv("https://phl.carto.com/api/v2/sql?filename=covid_deaths_by_race&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20FROM%20covid_deaths_by_race") %>%
    filter(!is.na(racial_identity)) %>%
    mutate(
      Percentage = round(count/sum(count), 3)*100,
      racial_identity = case_when(
        racial_identity == "UNKNOWN" ~ "Unknown",
        racial_identity == "AFRICAN AMERICAN" ~ "African American",
        racial_identity == "HISPANIC" ~ "Hispanic",
        TRUE ~ racial_identity
      )
    ) %>%
    rename(
      Race = racial_identity,
      Count = count,
      TimeStamp = etl_timestamp
    )

}
