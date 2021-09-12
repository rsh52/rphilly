#' Extract COVID-19 hospitalization data from source
#'
#' @description
#' \code{covid_hospitalizations()} returns a data set in list format pertaining to the various COVID Hospitalizations exports on OpenDataPhilly in list format. This package is not responsible for the raw dataset or ETL process from OpenDataPhilly. ETL for OpenDataPhilly is done on a daily basis.
#'
#' Accessible sub-datasets include:
#' \itemize{
#' \item{age}
#' \item{date}
#' \item{race}
#' \item{sex}
#' \item{zip}
#' }
#'
#' \strong{Note}: For aggregate data, counts less than 6 patients are rounded up to 6 to preserve patient health information anonyimity.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr mutate select case_when rename filter
#' @export
#' @references
#' \href{https://www.opendataphilly.org/dataset/covid-hospitalizations}{COVID Hospitalizations}
#'
#' \href{https://www.opendataphilly.org/dataset/covid-hospitalizations/resource/fc4d4c7d-392b-4da7-8a9e-cec0ccea8b6f}{COVID Hospitalizations by Age}
#'
#' \href{https://www.opendataphilly.org/dataset/covid-hospitalizations/resource/be0f3aa9-c4d5-461d-ac55-245872de69ba}{COVID Hospitalizations by Date}
#'
#' \href{https://www.opendataphilly.org/dataset/covid-hospitalizations/resource/9335e9ea-e1ca-46ce-bbcd-ac1d708cb125}{COVID Hospitalizations by Race}
#'
#' \href{https://www.opendataphilly.org/dataset/covid-hospitalizations/resource/78b9a4e4-daef-464b-a883-91efccef86f7}{COVID Hospitalizations by Sex}
#'
#' \href{https://www.opendataphilly.org/dataset/covid-hospitalizations/resource/a73874d4-311c-489f-a976-9bc0bf2942d2}{COVID Hospitalizations by ZIP}


covid_hospitalizations <- function(){

  list(
    date = read_csv("https://phl.carto.com/api/v2/sql?filename=covid_hospitalizations_by_date&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20FROM%20covid_hospitalizations_by_date", show_col_types = FALSE, progress = FALSE) %>%
      mutate(
        hospitalized = tolower(hospitalized)
      ),



    zip = read_csv("https://phl.carto.com/api/v2/sql?filename=covid_hospitalizations_by_zip&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20FROM%20covid_hospitalizations_by_zip", show_col_types = FALSE, progress = FALSE) %>%
      mutate(
        hospitalized = tolower(hospitalized)
      ),


    race = read_csv("https://phl.carto.com/api/v2/sql?filename=covid_hospitalizations_by_race&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20FROM%20covid_hospitalizations_by_race", show_col_types = FALSE, progress = FALSE) %>%
      mutate(
        hospitalized = tolower(hospitalized),
        racial_identity = case_when(
          racial_identity == "AMERICAN INDIAN" ~ "American Indian",
          racial_identity == "African American" ~ "African American",
          racial_identity == "PACIFIC ISLANDER" ~ "Pacific Islander",
          racial_identity == "NATIVE AMERICAN" ~ "Native American",
          racial_identity == "BLACK" ~ "Black",
          racial_identity == "HISPANIC" ~ "Hispanic",
          racial_identity == "OTHER" ~ "Other",
          racial_identity == "UNKNOWN" ~ "Unknown",
          racial_identity == "DECLINE" ~ "Declined to Specify",
          TRUE ~ racial_identity
        )
      )%>%
      rename(
        race = racial_identity
      ),


    age = read_csv("https://phl.carto.com/api/v2/sql?filename=covid_hospitalizations_by_age&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20FROM%20covid_hospitalizations_by_age", show_col_types = FALSE, progress = FALSE) %>%
      mutate(
        hospitalized = tolower(hospitalized)
      ),


    sex =read_csv("https://phl.carto.com/api/v2/sql?filename=covid_hospitalizations_by_sex&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20FROM%20covid_hospitalizations_by_sex", show_col_types = FALSE, progress = FALSE) %>%
      mutate(
        hospitalized = tolower(hospitalized)
      )
  )

}
