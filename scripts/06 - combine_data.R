#### Preamble ####
# Purpose: Explores data in an attempt to understand it more
# Author: Maria Mangru
# Date: September 2024
# Contact: maria.mangru@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
library(dplyr)
library(tidyverse)
library(readr)
library(knitr)
library(kableExtra)
library(scales)


### Load Data ###
X2022_2031_budget_data <- read_csv("data/analysis_data/2022-2031-budget_data.csv", col_types = cols(`Ward Number` = col_character()))
ward_data_clean <- read_csv("data/analysis_data/2023-WardProfile-Data.csv")


### Restructure ##
# Rename the first column to 'Category' for clarity
ward_profile_clean <- ward_data_clean |>
  rename(Category = `...1`) |> 
  select(-Toronto) |>            
  pivot_longer(
    cols = starts_with("Ward"),
    names_to = "Ward Number",
    values_to = "Value"
  ) |>
  pivot_wider(
    names_from = Category,
    values_from = Value
  )

View(ward_profile_clean)

# Function to clean dollar-formatted strings
clean_dollar <- function(x){
  as.numeric(str_replace_all(x, "[$,]", ""))
}

# Function to clean percentage-formatted strings
clean_percent <- function(x){
  as.numeric(str_replace(x, "%", ""))
}


# Apply cleaning functions to appropriate columns
ward_profile_clean <- ward_profile_clean |>
  mutate_at(vars(contains("$")), clean_dollar) |>   
  mutate_at(vars(contains("%")), clean_percent) |>
  mutate_at(vars(-`Ward Number`), ~ as.numeric(.))    


### Combine Data ###

# Perform a left join to combine funding data with ward demographics
combined_data <- X2022_2031_budget_data |>
  left_join(ward_profile_clean, by = "Ward Number")

combined_data <- combined_data |>
  group_by(`Ward Number`) |>
  summarise(
    Total_Funding = sum(across(all_of(year_cols)), na.rm = TRUE)
  ) |>
  arrange(desc(Total_Funding))

colnames(ward_profile_clean)

# Calculate Funding Per Capita
combined_data <- combined_data |>
  mutate(
    Funding_Per_Capita = Total_Funding / "Total Population"
  )

# Calculate Funding as a Percentage of Median Household Income
combined_data <- combined_data |>
  mutate(
    Funding_Per_Median_Income = Total_Funding / `Median total income of households in 2020 ($)`
  )

