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

question_4 = 
  survey %>%
  select(-`Start time`, -`Completion time`, -Email, -Name, -Colonne1, -Colonne2,
         -`What advice would you give your past-self or a young researcher from your country?`,
         -`Describe your dream of the future as a tropical scientist in a tweet (up to 280 characters).`) %>%
  unnest_tokens(word,
                `Tell us a short story about your background in science! Do you want to share an image or a video that represents your story as a tropical/subtropical ecologist? Go ahead!`)

question_4 %>%
  filter(Language == "EN") %>%
  anti_join(stop_words, 
            by=c("word"="word")) %>%
  left_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)

# End of script ----
