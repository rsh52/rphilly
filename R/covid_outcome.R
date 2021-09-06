#' Extract COVID-19 outcomes data from source
#'
#' @description The \code{covid_*} family of functions pull COVID-19 tracking
#' data from data from \url{https://www.opendataphilly.org} which provides .csv files taken from a CARTO SQL database API.
#'
#' \code{covid_outcome} pulls data from the COVID Tests and Cases registry and presents the output in a pre-processed, tidy dataset.
#'
#' Note that this extraction provides only aggregate summation of data from the start of recording to the ETL \code{TimeStamp} which generally takes place at 15:00:00 UTC each day.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr mutate select case_when rename
#' @export
#' @references
#' - COVID Tests and Cases: \url{https://www.opendataphilly.org/dataset/covid-cases}
#'
#' - COVID Tests by Outcome: \url{https://www.opendataphilly.org/dataset/covid-cases/resource/cdf4a96d-3655-49bc-8490-2a73260a29a7}

covid_outcome <- function() {

  read_csv("https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+covid_cases_by_outcome&filename=covid_cases_by_outcome&format=csv&skipfields=cartodb_id") %>%
    select(test_result,
           covid_outcome,
           count,
           etl_timestamp) %>%
    rename(Outcome = covid_outcome,
           TestResult = test_result,
           Count = count,
           TimeStamp = etl_timestamp) %>%
    mutate(
      Outcome = case_when(
        Outcome == "died" ~ "Died",
        Outcome == "positive" ~ "Positive",
        Outcome == "negative" ~ "Negative",
        TRUE ~ NA_character_),
      TestResult = case_when(
        TestResult == "positive" ~ "Positive",
        TestResult == "negative" ~ "Negative",
        TRUE ~ NA_character_
      ),
      Status = paste(TestResult, " - ", Outcome)
    )
}
