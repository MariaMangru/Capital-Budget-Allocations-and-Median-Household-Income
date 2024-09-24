#### Preamble ####
# Purpose: Cleans the raw data downloaded into a format which can be analyzed
# Author: Maria Mangru
# Date: September 2024
# Contact: maria.mangru@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
library(dplyr)
library(tidyverse)
library(readr)

#### Data Cleaning ####
# clean ward profile 
ward_profile <- read_csv("data/raw_data/2023-WardProfiles-2011-2021-CensusData.csv")

# remove trailing whitespaces
ward_profile <- ward_profile %>%
  mutate(across(where(is.character), str_trim))


# clean budget data 
budget_data <- read_csv("data/raw_data/2022-2031-capital-budget-and-plan-details.csv")

filtered_budget_data <- budget_data[-(which(budget_data$'Ward Number' %in% "CW")),]

view(filtered_budget_data)


#### Save data ####
write_csv(filtered_budget_data, "data/analysis_data/2022-2031-budget_data.csv")



