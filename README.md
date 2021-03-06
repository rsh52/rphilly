
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rphilly

<!-- badges: start -->

[![R-CMD-check](https://github.com/rsh52/rphilly/workflows/R-CMD-check/badge.svg)](https://github.com/rsh52/rphilly/actions)
[![Codecov test
coverage](https://codecov.io/gh/rsh52/rphilly/branch/master/graph/badge.svg)](https://codecov.io/gh/rsh52/rphilly?branch=master)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

``` r
knitr::include_graphics("img/rphilly.png")
```

<img src="img/rphilly.png" width="250px" />

The goal of `rphilly` is to provide convenient helper functions and
pre-processed datasets from
[OpenDataPhilly.org](https://www.opendataphilly.org/). This package is
in no way responsible for the actual recording, updating, or contents of
the raw datasets. (It also has no relation to the NHL, but we do respect
our favorite orange jawn)

## Installation

You can install the development version of `rphilly` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("rsh52/rphilly")
```

> This package is still in development.

## `rphilly`: COVID-19

The following functions are available for COVID-19 data extraction, with
data supplied from [several
datasets](https://www.opendataphilly.org/organization/city-of-philadelphia?q=covid&sort=score+desc%2C+metadata_modified+desc):

-   `covid_tests_cases`
-   `covid_hospitalizations`
-   `covid_vaccinations`
