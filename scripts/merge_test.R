#### Preamble ####
# Purpose: Merge demographic and budget data 
# Author: Maria Mangru
# Date: September 2024
# Contact: maria.mangru@mail.utoronto.ca
# License: MIT




#### Workspace Setup ####
library(dplyr)
library(tidyr)
library(readr)
library(knitr)
library(kableExtra)
library(scales)
library(stringr)
library(janitor)
library(ggplot2)

### Data Handling ###

# Read in the budget data
budget_data <- read_csv("data/analysis_data/20221-2031-budget_data.csv")

# Read in the demographic data
demographic_data <- read_csv("data/analysis_data/2023-WardProfile-Data.csv")

# View the structure of the datasets
str(budget_data)
str(demographic_data)


# Identify year columns in budget data
year_cols <- as.character(2022:2031)


# Aggregate total funding per ward number
total_funding_ward <- budget_data |>
  group_by(`Ward Number`) |>
  summarise(
    total_funding = sum(across(all_of(year_cols)), na.rm = TRUE)
  ) |>
  arrange(desc(total_funding))

# View total funding per ward
print(total_funding_ward)


# Pivot demographic data from wide to long format
demographic_long <- demographic_data |>
  pivot_longer(
    cols = -Category,
    names_to = "Ward Number",
    values_to = "Value"
  )


# Pivot demographic data back to wide format to get one row per ward
demographic_wide <- demographic_long |>
  pivot_wider(
    names_from = Category,
    values_from = Value
  )

demographic_wide$`Ward Number` <- as.numeric(as.character(demographic_wide$`Ward Number`))


as.numeric(as.character(total_funding_ward$`Ward Number`))

sapply(total_funding_ward, class)
sapply(demographic_wide, class)

# Merge budget data with demographic data
merged_data <- total_funding_ward |>
  left_join(demographic_wide, by = "Ward Number")


colnames(merged_data)



### Tables and Visualizations ###

# Select relevant columns for the table
merged_table <- merged_data |>
  select(
    `Ward Number`,
    `total_funding`,
    `Total Population`,
    `Median total income of households in 2020 ($)`,
    `Median total income of one-person households in 2020 ($)`,
    `Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)`
  )


# Create a table
merged_table |>
  arrange(`Ward Number`) |>
  kable(
    col.names = c("Ward Number", "Total Funding", "Total Population", "Household Median Income", "Individual Median Income","Low Income Prevalence"),
    format = "html",
    caption = "Total Funding and Demographic Data per Ward"
  ) |>
  kable_styling(full_width = FALSE, position = "center")



