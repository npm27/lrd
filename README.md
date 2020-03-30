# lrd - Lexical Response Data

## Overview

The `lrd` package provides tools for quickly and accurately processing the large amounts of lexical response data that are typically generated from cued-recall tests, while also being able to control for minor errors in participant responses.

## To install:

The following steps will install the development version of `lrd`. CRAN release coming soon!

  1. Install devtools if you do not have it. This package lets install packages hosted on GitHub. `install.packages(devtools)`

  2. With devtools installed, you can then install domo by executing the following: `devtools::install_github("npm27/lrd")`

  3. Load the library to get started! `library(lrd)`

## Scoring data:

This package uses two functions for scoring data.

  1. `percent_match()` This function computes the percentage of overlapping characters shared between two strings.
	
  2. `score_recall()` This function determines whether or not two strings match based on a user defined percentage of matching characters.

Detailed descriptions of each function are available in the documentation and in the [manuscript](https://osf.io/g96a7/).


You can also use the [Shiny app](https://npm27.shinyapps.io/lrdshiny/)!
