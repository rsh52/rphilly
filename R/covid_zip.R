#' Extract COVID-19 date data from source
#'
#' @description The \code{covid_*} family of functions pull COVID-19 tracking
#' data from data from \url{https://www.opendataphilly.org} which provides .csv files taken from a CARTO SQL database API.
#'
#' \code{covid_zip} pulls test result and death data per date and ZIP code from the COVID Tests and Cases registry as well as the COVID Deaths registry and presents the output in a pre-processed, tidy dataset.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr mutate select case_when rename
#' @export
#' @references
#' - COVID Tests and Cases: \url{https://www.opendataphilly.org/dataset/covid-cases}
#'
#' - COVID Tests by ZIP: \url{https://www.opendataphilly.org/dataset/covid-cases/resource/d4d1e48a-d401-405c-972b-c45292c3d4f5}
#'
#' - COVID Deaths: \url{https://www.opendataphilly.org/dataset/covid-deaths}
#'
#' - COVID Deaths by ZIP: \url{https://www.opendataphilly.org/dataset/covid-deaths/resource/7da31c98-c2ff-4fea-b313-2a40230d4810}

covid_zip <- function(){

  covid_zip <- read_csv("https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+covid_cases_by_zip&filename=covid_cases_by_zip&format=csv&skipfields=cartodb_id") %>%
    select(covid_status, zip_code, count, etl_timestamp) %>%
    rename(ZIP = zip_code,
           Outcome = covid_status,
           Count = count,
           TimeStamp = etl_timestamp)

  covid_zip_died <- read_csv("https://phl.carto.com/api/v2/sql?filename=covid_deaths_by_zip&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20FROM%20covid_deaths_by_zip") %>%
    rename(ZIP = zip_code,
           Outcome = covid_outcome,
           Count = count,
           TimeStamp = etl_timestamp)

  rbind(covid_zip, covid_zip_died)
}


