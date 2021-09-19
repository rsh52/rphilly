#' Extract COVID-19 tests and cases data from source
#'
#' @description
#' \code{covid_tests_cases()} returns a data set in list format pertaining to the various COVID Tests and Cases as well as the COVID Deaths exports on OpenDataPhilly in list format. This package is not responsible for the raw dataset or ETL process from OpenDataPhilly. ETL for OpenDataPhilly is done on a daily basis.
#'
#' Accessible sub-datasets include:
#' \itemize{
#' \item{age}
#' \item{date}
#' \item{outcome}
#' \item{race}
#' \item{sex}
#' \item{zip}
#' }
#'
#' \strong{Note}: For aggregate data, counts less than 6 patients are rounded up to 6 to preserve patient health information anonyimity.
#'
#' \strong{Note}: The author of this package determined it would be best to combine elements of the Deaths and Tests and Cases datasets for ease of use here.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr mutate select case_when rename transmute summarise group_by filter %>%
#' @export
#' @references
#' \href{https://www.opendataphilly.org/dataset/covid-hospitalizations}{COVID Tests and Cases}
#'
#' \href{https://www.opendataphilly.org/dataset/covid-deaths}{COVID Deaths}
#'
#' \href{https://www.opendataphilly.org/dataset/covid-cases/resource/86d7c7f5-b348-4ce2-bbb3-8d8e6f6dec98}{COVID Tests and Cases by Age}
#'
#' \href{https://www.opendataphilly.org/dataset/covid-deaths/resource/33e68765-d07a-410a-9736-ac28fd587ce6}{COVID Deaths by Age}
#'
#' \href{https://www.opendataphilly.org/dataset/covid-cases/resource/85fead1a-0ec3-4856-98a9-27ba4fa941ee}{COVID Tests and Cases by Date}
#'
#' \href{https://www.opendataphilly.org/dataset/covid-deaths/resource/45760e22-e2de-456f-ad0f-6c491b0f2e42}{COVID Deaths by Date}
#'
#' \href{https://www.opendataphilly.org/dataset/covid-hospitalizations/resource/9335e9ea-e1ca-46ce-bbcd-ac1d708cb125}{COVID Tests and Cases by Race}
#'
#' \href{https://www.opendataphilly.org/dataset/covid-cases/resource/cdf4a96d-3655-49bc-8490-2a73260a29a7}{COVID Tests and Cases by Outcome}
#'
#' \href{https://www.opendataphilly.org/dataset/covid-deaths/resource/c053a703-d99c-4003-93ca-caa5db17369a}{COVID Deaths by Race}
#'
#' \href{https://www.opendataphilly.org/dataset/covid-cases/resource/856a0639-334c-43c2-a53b-1b3f5611fbda}{COVID Tests and Cases by Sex}
#'
#' \href{https://www.opendataphilly.org/dataset/covid-cases/resource/d4d1e48a-d401-405c-972b-c45292c3d4f5}{COVID Tests and Cases by ZIP}
#'
#' \href{https://www.opendataphilly.org/dataset/covid-deaths/resource/7da31c98-c2ff-4fea-b313-2a40230d4810}{COVID Deaths by ZIP}


covid_tests_cases <- function(){

  list(
    age = rbind(
      read_csv("https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+covid_cases_by_age&filename=covid_cases_by_age&format=csv&skipfields=cartodb_id",
               show_col_types = FALSE, progress = FALSE) %>%
        select(
          age,
          count,
          etl_timestamp
        ) %>%
        mutate(
          sex = "all",
          outcome = "positive"
        ),
      read_csv("https://phl.carto.com/api/v2/sql?filename=covid_deaths_by_age&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20FROM%20covid_deaths_by_age",
               show_col_types = FALSE, progress = FALSE) %>%
        select(
          age,
          count,
          etl_timestamp,
          gender
        ) %>%
        rename(
          sex = gender
        ) %>%
        mutate(
          outcome = "died"
        )
    ),

    date =  rbind(

      read_csv("https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+covid_cases_by_date&filename=covid_cases_by_date&format=csv&skipfields=cartodb_id",
               show_col_types = FALSE, progress = FALSE) %>%
        select(collection_date,
               count,
               test_result,
               etl_timestamp) %>%
        rename(result_date = collection_date,
               outcome = test_result),

      read_csv("https://phl.carto.com/api/v2/sql?filename=covid_deaths_by_date&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20FROM%20covid_deaths_by_date",
               show_col_types = FALSE, progress = FALSE) %>%
        select(clinical_date_of_death,
               count,
               covid_outcome,
               etl_timestamp) %>%
        rename(result_date = clinical_date_of_death,
               outcome = covid_outcome) %>%
        mutate(
          outcome = tolower(outcome)
        )

    ),

    sex = rbind(
      read_csv("https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+covid_cases_by_sex&filename=covid_cases_by_Gender&format=csv&skipfields=cartodb_id",
               show_col_types = FALSE, progress = FALSE) %>%
        select(
          sex,
          count,
          etl_timestamp
        ) %>%
        transmute(
          sex = tolower(sex),
          count,
          outcome = "positive",
          etl_timestamp
        ),

      read_csv("https://phl.carto.com/api/v2/sql?filename=covid_deaths_by_age&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20FROM%20covid_deaths_by_age",
               show_col_types = FALSE, progress = FALSE) %>%
        select(
          age,
          count,
          etl_timestamp,
          gender
        ) %>%
        rename(
          sex = gender
        ) %>%
        mutate(
          sex = tolower(sex)
        ) %>%
        group_by(sex) %>%
        summarise(count = sum(count),
                  etl_timestamp = unique(etl_timestamp)) %>%
        mutate(outcome = "died")
    ),

    outcome = read_csv("https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+covid_cases_by_outcome&filename=covid_cases_by_outcome&format=csv&skipfields=cartodb_id",
                       show_col_types = FALSE, progress = FALSE) %>%
      select(test_result,
             covid_outcome,
             count,
             etl_timestamp) %>%
      rename(outcome = covid_outcome) %>%
      mutate(
        status = paste(test_result, " - ", outcome)
      ),

    zip = rbind(
      read_csv("https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+covid_cases_by_zip&filename=covid_cases_by_zip&format=csv&skipfields=cartodb_id",
               show_col_types = FALSE, progress = FALSE) %>%
        select(covid_status,
               zip_code,
               count,
               etl_timestamp) %>%
        rename(outcome = covid_status) %>%
        mutate(
          outcome = case_when(
            outcome == "NEG" ~ "negative",
            outcome == "POS" ~ "positive",
            outcome == "DIED" ~ "died"
          ),
        ),

      read_csv("https://phl.carto.com/api/v2/sql?filename=covid_deaths_by_zip&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20FROM%20covid_deaths_by_zip",
               show_col_types = FALSE, progress = FALSE) %>%
        rename(outcome = covid_outcome) %>%
        mutate(
          outcome = case_when(
            outcome == "NEG" ~ "negative",
            outcome == "POS" ~ "positive",
            outcome == "DIED" ~ "died"
          ),
        )
    ),

    race = rbind(
      read_csv("https://phl.carto.com/api/v2/sql?filename=covid_cases_by_race&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20FROM%20covid_cases_by_race",
               show_col_types = FALSE, progress = FALSE) %>%
        filter(!is.na(racial_identity)) %>%
        mutate(
          percentage = round(count/sum(count),3)*100,
          racial_identity = tolower(racial_identity),
          outcome = "positive"
        ) %>%
        rename(
          race = racial_identity
        ),

      read_csv("https://phl.carto.com/api/v2/sql?filename=covid_deaths_by_race&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20FROM%20covid_deaths_by_race",
               show_col_types = FALSE, progress = FALSE) %>%
        filter(!is.na(racial_identity)) %>%
        mutate(
          percentage = round(count/sum(count), 3)*100,
          racial_identity = tolower(racial_identity),
          outcome = "died"
        ) %>%
        rename(
          race = racial_identity
        )
    )
  )
}
