#### Preamble ####
# Purpose: Clean the survey data downloaded from Democracy Fund + UCLA Nationscape
# Author: Yinuo zhang
# Data: 27 April 2022
# Contact: Yinuo.zhang@utoronto.ca 
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
library(labelled)
#Read in the raw data. 
raw_data <-  read_dta("inputs/data/ns20210112.dta") %>% 
  to_factor() %>% 
  select(age,
         gender, 
         hispanic,
         race_ethnicity,
         household_income,
         education,
         state,
         vote_2020_retro,
         news_sources_facebook,
         vote_2016,
         twitter_ban
  )

#Create education and incomes match table
educations <- names(table(raw_data$education))
incomes <- names(table(raw_data$household_income))

#only use votes for "Joe Biden", "Donald Trump"
#for"Joe Biden"= 0, "Donald Trump" = 1
#recode education 
#recode race 
#recode age 
#recode hispanic 
#recode income 
#recode stateicp
#fill NA to district of columbia
#convert to factor types

raw_data_cleaned <- raw_data %>%
  filter(vote_2020_retro %in% c("Joe Biden", "Donald Trump")) %>% 
  mutate(Trump2020 = as.integer(vote_2020_retro == "Donald Trump"),
         Trump2016 = as.integer(vote_2016 == "Donald Trump")) %>% 
  mutate(
    education = case_when(
      education %in% educations[1:5] ~ "High school or Lower",
      education %in% educations[6:8] ~ "BA or lower",
      education %in% educations[9:11] ~ "Above BA"
    )) %>%  mutate(
      incomegroup = case_when(
        household_income %in% incomes[1:6] ~ "Median or below",
        !household_income %in% incomes[1:6] ~ "Above median",
      )
    ) %>%
  mutate(
    race = case_when(
      race_ethnicity == "White" ~ "white",
      race_ethnicity == "Black, or African American" ~ "black",
      ! race_ethnicity %in% c("White" ,"Black, or African American" ) ~ "other"
    )) %>% 
  mutate(
    agegroup = case_when(
      age <= 35 ~ "18-35",
      age <= 50 ~ "36-50",
      age <= 65 ~ "50-65",
      age >  65 ~ "65+"
    ))  %>% 
  
  mutate(
    hispanic = ifelse(hispanic == "Not Hispanic",
                      "not hispanic",
                      "hispanic"),
  ) %>% mutate(
    twitter_ban = ifelse(twitter_ban %in% c("Very worried","Somewhat worried"),
                         "worried","not worried")
  ) %>% 
  left_join( tibble(stateicp = state.name,
                    state = state.abb)) %>% 
  mutate(stateicp = tolower(stateicp),
         stateicp = replace_na(stateicp, "district of columbia")) %>%
  mutate(
         race = factor(race),
         incomegroup = factor(incomegroup),
         stateicp = factor(stateicp),
         education = factor(education),
         hispanic  = factor(hispanic )
  )%>% drop_na() %>%
  select(
   age,
   agegroup ,
   gender,
   race,
   incomegroup,
   education,
   hispanic ,
   stateicp,
   Trump2020 ,
   Trump2016,
   news_sources_facebook,
   twitter_ban
  )

#save to outputs
write_rds(raw_data_cleaned, "outputs/paper/data/2020.rds")



