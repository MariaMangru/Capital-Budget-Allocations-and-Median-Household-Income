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

## Ward Profile Data ## 
# Clean ward profile 
ward_profile <- read_csv("data/raw_data/2023-WardProfiles-2011-2021-CensusData.csv")

# Remove trailing whitespaces from character columns
ward_profile_clean <- ward_profile |>
  mutate(across(where(is.character), str_trim))

# Save cleaned ward profile data 
write_csv(ward_profile_clean, "data/analysis_data/2023-WardProfile-Data.csv")


## 2022 - 2031 Budget Data ##
# Clean budget data 
budget_data <- read_csv("data/raw_data/2022-2031-capital-budget-and-plan-details.csv")

# Remove rows where 'Ward Number' is "CW"
filtered_budget_data <- budget_data[-(which(budget_data$'Ward Number' %in% "CW")),]

# Identify year columns
year_cols <- as.character(2022:2031)

# Function to clean amount 
clean_funding <- function(x) {
  x_clean <- ifelse(x == "-", "0", x)
  x_clean <- str_replace_all(x_clean, ",", "")
  as.numeric(x_clean)
}

# Apply the cleaning function to year columns
budget_data_clean <- filtered_budget_data |>
  mutate(across(all_of(year_cols), clean_funding))


# Save cleaned budget data 
write_csv(budget_data_clean, "data/analysis_data/2022-2031-budget_data.csv")


