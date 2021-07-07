#' ------------------------------------------------------------------#
#'  PARTICIPANT AFFILIATION MAP
#'  script for importing and prepearing data to be used for the Alumni
#'  shiny app. This data is to be used for the PFTC website.
#'  Developed by: TANYA STRYDOM
#'  Reachable at tanya.strydom@icloud.com
#'  For script queries and bug fixes if needed
#' ------------------------------------------------------------------#


### 0) Preamble ----
### >> a) Dependencies ----
library(tidyverse)
library(gsheet)
library(stringr)
library(tidytext)

### 1) Dataframes ----
### >> a) Surveys ----

survey <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1V4AdCLm6UgTqO7UKjBFwAogBvPi1zWEjBZ37A2f4dcI/edit?usp=sharing')



# End of script ----
