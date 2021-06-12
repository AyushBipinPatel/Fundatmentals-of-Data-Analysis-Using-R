#### This script is to clean the survey data ans annonamize any personal identifiers###



# Library -----------------------------------------------------------------

library(here)
library(tidyverse)
library(janitor)



# Data Import -------------------------------------------------------------

data_survey <- read_csv(here("data/survey_response.csv"))


data_survey %>% 
  select(-Username) %>% 
  write_csv(here("data/survey_response.csv"))
                        
data_survey %>% 
  select(-Username) -> data_survey


# Data Cleaning -----------------------------------------------------------

### appropriate column names

names_column <- c("timestamp", "age_y","gender",
                  "fam_members","height_cm", "weight_kg",
                  "prefrence_entertainment", "play_mobile_games",
                  "mobile_game_addict","price_mobile","brand_mobile",
                  "cost_last_internet_package", "fam_mem_eligible_vaccinated",
                  "satisfaction_central_gov","what_first_after_pandemic",
                  "online_class_after_pandemic","prefrence_food",
                  "prefrence_dish","cost_dinner_outing")


colnames(data_survey) <- names_column

### Column cleaning

#### timestamp is auto generated, no need for cleaning.

#### Age should be in years

unique(data_survey$age_y)

data_survey %>% 
  mutate(
    age_y = ifelse(
      str_detect(age_y,"[:digit:]+"),
      age_y,
      NA
    ),
    age_y = str_remove_all(age_y,"[:alpha:]+"),
    age_y = ifelse(
      str_detect(age_y,"\\."),
      2021 - as.numeric(str_extract(age_y,"(?<=\\.)[:digit:]{4}(?=\\.)")),
      str_extract(age_y,"[:digit:]{2}(?=\\.)")
    )
  ) %>% 
  pull(age_y) %>% 
  unique()






























