#### Preamble ####
# Purpose: Cleans the raw data downloaded, into a format which can be analyzed
# Author: Maria Mangru
# Date: 27 September 2024
# Contact: maria.mangru@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
library(dplyr)
library(tidyverse)
library(readr)
library(stringr)


#### Data Cleaning ####

## Ward Profile Data ## 

# Clean ward profile 
ward_profile <- read_csv("data/raw_data/2023-WardProfiles-2011-2021-CensusData.csv")

# Remove rows with NAs
ward_profile <- na.omit(ward_profile)

# Save cleaned ward profile data 
write_csv(ward_profile, "data/analysis_data/clean_ward_data.csv")



## 2022 - 2031 Budget Data ##

# Clean budget data 
budget_data <- read_csv("data/raw_data/2022-2031-capital-budget-and-plan-details.csv")

# Remove rows where 'Ward Number' is "CW"
budget_data <- budget_data |>
  filter(`Ward Number` != "CW")

# Identify year columns
year_cols <- as.character(2022:2031)

# Function to clean amount 
clean_funding <- function(x) {
  x_clean <- ifelse(x == "-", "0", x)
  x_clean <- str_replace_all(x_clean, ",", "")
  as.numeric(x_clean)
}

# Apply the cleaning function to year columns
budget_data_clean <- budget_data |>
  mutate(across(all_of(year_cols), clean_funding))


# Save cleaned budget data 
write_csv(budget_data_clean, "data/analysis_data/clean_budget_data.csv")


