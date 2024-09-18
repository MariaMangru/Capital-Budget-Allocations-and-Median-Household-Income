#### Preamble ####
# Purpose: Cleans the raw plane data recorded by two observers..... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 6 April 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]

#### Workspace setup ####
library(dplyr)
library(tidyverse)
library(readr)

#### Clean data ####
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
write_csv(filtered_budget_data, "data/analysis_data/test.csv")
