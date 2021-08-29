#' Extract COVID-19 outcomes data from source
#'
#' @description The \code{covid_*} family of functions pull COVID-19 tracking
#' data from OpenDataPhilly.org which provides .csv files taken from a CARTO SQL database API.
#'
#' \code{covid_outcome} pulls data from the COVID Tests and Cases registry and presents the output in a pre-processed, tidy dataset.
#'
#' Note that this extraction provides only a sum total since the start of recording and not a summary over time.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr mutate select case_when rename
#' @export


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
