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
library(wordcloud)
library(reshape2)

### >> b) Colours ----

sent_pal = c('red', 'grey50', 'green')

### 1) Dataframes ----

survey <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1V4AdCLm6UgTqO7UKjBFwAogBvPi1zWEjBZ37A2f4dcI/edit?usp=sharing')

### >> a) Question 4 ----

# Tell us a short story about your background in science! Do you want to 
# share an image or a video that represents your story as a tropical/subtropical 
# ecologist? Go ahead!

question_4 = 
  survey %>%
  select(-`Start time`, -`Completion time`, -Email, -Name, -Colonne1, -Colonne2,
         -`What advice would you give your past-self or a young researcher from your country?`,
         -`Describe your dream of the future as a tropical scientist in a tweet (up to 280 characters).`) %>%
  unnest_tokens(word,
                `Tell us a short story about your background in science! Do you want to share an image or a video that represents your story as a tropical/subtropical ecologist? Go ahead!`) %>%
  filter(`Translated to` == "EN") %>%
  anti_join(stop_words, 
            by=c("word"="word")) %>%
  left_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  rename(`freq` = `n`) %>%
  mutate(colour = as.character(case_when(sentiment == "negative" ~ "red",
                                         sentiment == "positive" ~ "#E00008",
                                         is.na(sentiment) ~ "white"))) %>%
  mutate(sentiment = ifelse(is.na(sentiment),
                            'neutral', sentiment)) %>% #rename 'non-sentiment'
  filter(str_detect(word,
                    '[:alpha:]')) %>% #remove numbers
  acast(word ~ sentiment, value.var = "freq", fill = 0) #%>% #reshape
# comparison.cloud(colors = sent_pal,
#                  max.words = 100, scale=c(3.5,0.50))

### >> b) Question 5 ----

# What advice would you give your past-self or a young 
# researcher from your country?

question_5 = 
  survey %>%
  select(-`Start time`, -`Completion time`, -Email, -Name, -Colonne1, -Colonne2,
         -`Tell us a short story about your background in science! Do you want to share an image or a video that represents your story as a tropical/subtropical ecologist? Go ahead!`,
         -`Describe your dream of the future as a tropical scientist in a tweet (up to 280 characters).`) %>%
  unnest_tokens(word,
                `What advice would you give your past-self or a young researcher from your country?`) %>%
  filter(`Translated to` == "EN") %>%
  anti_join(stop_words, 
            by=c("word"="word")) %>%
  left_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  rename(`freq` = `n`) %>%
  mutate(colour = as.character(case_when(sentiment == "negative" ~ "red",
                                         sentiment == "positive" ~ "#E00008",
                                         is.na(sentiment) ~ "white"))) %>%
  mutate(sentiment = ifelse(is.na(sentiment),
                            'neutral', sentiment)) %>% #rename 'non-sentiment'
  filter(str_detect(word,
                    '[:alpha:]')) %>% #remove numbers
  acast(word ~ sentiment, value.var = "freq", fill = 0) #%>% #reshape
# comparison.cloud(colors = sent_pal,
#                  max.words = 100, scale=c(3.5,0.50))


### >> b) Question 6 ----

# Describe your dream of the future as a tropical scientist 
# in a tweet (up to 280 characters).

question_6 = 
  survey %>%
  select(-`Start time`, -`Completion time`, -Email, -Name, -Colonne1, -Colonne2,
         -`Tell us a short story about your background in science! Do you want to share an image or a video that represents your story as a tropical/subtropical ecologist? Go ahead!`,
         -`What advice would you give your past-self or a young researcher from your country?`) %>%
  unnest_tokens(word,
                `Describe your dream of the future as a tropical scientist in a tweet (up to 280 characters).`) %>%
  filter(`Translated to` == "EN") %>%
  anti_join(stop_words, 
            by=c("word"="word")) %>%
  left_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  rename(`freq` = `n`) %>%
  mutate(colour = as.character(case_when(sentiment == "negative" ~ "red",
                                         sentiment == "positive" ~ "#E00008",
                                         is.na(sentiment) ~ "white"))) %>%
  mutate(sentiment = ifelse(is.na(sentiment),
                            'neutral', sentiment)) %>% #rename 'non-sentiment'
  filter(str_detect(word,
                    '[:alpha:]')) %>% #remove numbers
  acast(word ~ sentiment, value.var = "freq", fill = 0) #%>% #reshape
  # comparison.cloud(colors = sent_pal,
  #                  max.words = 100, scale=c(3.5,0.50))


# End of script ----
