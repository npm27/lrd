# lrd - Lexical Response Data

## Overview

The `lrd` package provides tools for quickly and accurately processing the large amounts of lexical response data that are typically generated from recall tests, while also being able to control for minor errors in participant responses.

## To install:

The following steps will install the development version of `lrd`. CRAN release coming soon!

  1. Install devtools if you do not have it. This package lets install packages hosted on GitHub. `install.packages("devtools")`

  2. With devtools installed, you can then install lrd by executing the following: `devtools::install_github("npm27/lrd")`

  3. Load the library to get started! `library(lrd)`

## Scoring data:

This package contains four functions for scoring data.

  1. `prop_correct_cued()` This function can be used to score cued-recall data.

  2. `prop_correct_free()` This function can be used to score data from free recall studies.

  3. `prop_correct_multiple()` This function can be used to score data from free recall studies when participants study multiple or random lists.
  
  4. `prop_correct_sentence()` This function can be used to score sentence recall.

In additionally, `arrange_data()` can be used to convert wide format data to long format prior to scoring.

Detailed descriptions of each function are available in the documentation and in the [manuscript](https://osf.io/r942y/).

You can also use the [Shiny app](https://npm27.shinyapps.io/lrd_shiny/)! For step-by-step instructions on using the Shiny application, see the [how-to-guide](https://www.macapsych.com/lexical-re).

## Contact:

We happily take comments and suggestions. Please email nicholas.maxwell@usm.edu.
