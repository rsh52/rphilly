#' Extract COVID-19 vaccination data from source
#'
#' @description
#' \code{covid_vaccinations()} returns a data set in list format pertaining to the various COVID vaccinations exports on OpenDataPhilly in list format. This package is not responsible for the raw dataset or ETL process from OpenDataPhilly. ETL for OpenDataPhilly is done on a daily basis.
#'
#' Accessible sub-datasets include:
#' \itemize{
#' \item{age}
#' \item{date}
#' \item{outcome}
#' \item{partially_vaccinated}
#' \item{fully_vaccinated}
#' \item{race}
#' \item{sex}
#' \item{zip}
#' }
#'
#' \strong{Note}: For aggregate data, counts less than 6 patients are rounded up to 6 to preserve patient health information anonyimity.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr mutate select case_when rename transmute summarise group_by filter %>%
#' @export
#' @references
#' \href{https://www.opendataphilly.org/dataset/covid-vaccinations}{COVID Vaccinations}
#'
#' \href{https://www.opendataphilly.org/dataset/covid-vaccinations/resource/6fab01e7-35cd-4df0-b148-bdc255ade275}{COVID Vaccination Census Tract}
#'
#' \href{https://www.opendataphilly.org/dataset/covid-vaccinations/resource/ae0d2205-de30-4663-8e99-68092fb41523}{COVID Vaccination by Age}
#'
#' \href{https://www.opendataphilly.org/dataset/covid-vaccinations/resource/60775668-9ae5-473c-8e76-9860bb74f48c}{COVID Vaccination by Race}
#'
#' \href{https://www.opendataphilly.org/dataset/covid-vaccinations/resource/b4db0f64-f51a-43fa-87bc-359b307d2f54}{COVID Vaccination by Sex}
#'
#' \href{https://www.opendataphilly.org/dataset/covid-vaccinations/resource/84f9c309-0560-4ec7-acc4-65fbb14239c9}{COVID Vaccination Total}
#'
#' \href{https://www.opendataphilly.org/dataset/covid-vaccinations/resource/87ac5b4e-8491-41e3-8cf0-5bfebba2e3a0}{COVID Vaccination by ZIP}

covid_vaccinations <- function(){

  list(
    census_tract = read_csv("https://phl.carto.com/api/v2/sql?filename=covid_vaccines_by_census_tract&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20FROM%20covid_vaccines_by_census_tract",
                            show_col_types = FALSE, progress = FALSE),

    age = read_csv("https://phl.carto.com/api/v2/sql?filename=covid_vaccines_by_age&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20FROM%20covid_vaccines_by_age",
                   show_col_types = FALSE, progress = FALSE),


    race = read_csv("https://phl.carto.com/api/v2/sql?filename=covid_vaccines_by_race&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20FROM%20covid_vaccines_by_race",
                   show_col_types = FALSE, progress = FALSE) %>%
      rename(
        race = racial_identity
      ),

    sex = read_csv("https://phl.carto.com/api/v2/sql?filename=covid_vaccines_by_sex&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20FROM%20covid_vaccines_by_sex",
                    show_col_types = FALSE, progress = FALSE) %>%
      mutate(
        sex = case_when(sex == "M" ~ "Male",
                        sex == "F" ~ "Female",
                        sex == "T" ~ "Trans",
                        TRUE ~ sex)
      ),

    total = read_csv("https://phl.carto.com/api/v2/sql?filename=covid_vaccine_totals&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20FROM%20covid_vaccine_totals",
                     show_col_types = FALSE, progress = FALSE),

    zip = read_csv("https://phl.carto.com/api/v2/sql?filename=covid_vaccines_by_zip&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20FROM%20covid_vaccines_by_zip",
                     show_col_types = FALSE, progress = FALSE)

  )




}
