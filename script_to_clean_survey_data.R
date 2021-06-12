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
      2021 - as.numeric(str_extract(age_y,"(?<=\\.)[:digit:]{4}")),
      str_extract(age_y,"[:digit:]{2}")
    ),
    age_y = as.numeric(age_y),
    fam_members = str_remove_all(fam_members,
                                 "[:alpha:]+"),
    fam_members = as.numeric(
      str_extract(fam_members,"[:digit:]{1,2}")
    ),
    height_cm = ifelse(
      !str_detect(height_cm,"[:alpha:]+"),
      height_cm,
      ifelse(str_detect(height_cm,"cm"),
             str_remove_all(height_cm, 
                            "[:alpha:]+"),
             height_cm)
    ),
    height_cm = ifelse(
      str_detect(height_cm,"[:alpha:]+"),
      NA,
      ifelse(
        str_detect(height_cm,"'"),
        NA,
        height_cm
      )),
    height_cm = as.numeric(height_cm),
    height_cm = ifelse(height_cm < 90,
                       NA,
                       height_cm),
    weight_kg = as.numeric(
      ifelse(
        str_detect(weight_kg,"[:alpha:]+"),
        str_remove_all(weight_kg,"[:alpha:]+"),
        weight_kg
      )
    ),
    prefrence_entertainment = fct_lump_min(as.factor(prefrence_entertainment),
                                           min = 5),
    price_mobile = as.numeric(price_mobile),
    brand_mobile = fct_lump_min(as.factor(brand_mobile),
                                           min = 5),
    cost_last_internet_package = as.numeric(
      str_extract(
        cost_last_internet_package,"[:digit:]+"
      )
    ),
    fam_mem_eligible_vaccinated = as.numeric(
      fam_mem_eligible_vaccinated
    ),
    prefrence_dish = fct_lump_min(as.factor(prefrence_dish),
                                min = 5),
    cost_dinner_outing = as.numeric(cost_dinner_outing)
  ) -> data_survey


# saving as csv -----------------------------------------------------------

data_survey %>% 
  write_csv(here("data/cleaned_survey_data.csv"))





























