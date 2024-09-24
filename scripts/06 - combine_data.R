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
# Budget funding data
budget_data_clean <- read_csv("data/analysis_data/2022-2031-budget_data.csv")

# Demographic data
demographic_data <- read_csv("data/analysis_data/demographic_data.csv")

# Ensure that `Ward Number` is consistent in both datasets
budget_data_clean <- budget_data_clean |>
  mutate(`Ward Number` = as.integer(`Ward Number`))

demographic_data <- demographic_data |>
  rename(`Ward Number` = `...1`) |>
  mutate(`Ward Number` = as.integer(`Ward Number`))


### Restructure ###
# Reshape budget data to long format 
year_cols <- as.character(2022:2031)

budget_data_long <- budget_data_clean %>%
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "Year",
    values_to = "Funding"
  )

# Summarize total funding per ward
total_funding_ward <- budget_data_clean %>%
  group_by(`Ward Number`) %>%
  summarise(
    Total_Funding = sum(across(all_of(year_cols)), na.rm = TRUE)
  ) %>%
  arrange(desc(Total_Funding))

# Summarize total funding per ward and per program/agency
total_funding_program_ward <- budget_data_clean %>%
  group_by(`Ward Number`, `Program/Agency Name`) %>%
  summarise(Total_Funding = sum(across(all_of(year_cols)), na.rm = TRUE)) %>%
  arrange(`Ward Number`, desc(Total_Funding))

# Merge total funding with demographic data
funding_demographics <- total_funding_ward %>%
  left_join(demographic_data, by = "Ward Number")

# Inspect the merged data
head(funding_demographics)








