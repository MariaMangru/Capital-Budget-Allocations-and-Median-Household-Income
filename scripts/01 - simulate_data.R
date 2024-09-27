#### Preamble ####
# Purpose:Simulate ward profile and budget data
# Author: Maria Mangru
# Date: 27 September 2024
# Contact: maria.mangru@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(tidyverse)
library(readr)

#### Simulate data ####

# Simulate ward profile data 
simulate_ward_profile <- function() {
  set.seed(403) # for reproducibility 
  
  ward_numbers <- paste0("Ward", 1:25)
  census_years <- 2011:2021
  demographics <- c("Poulation", "Median Age", "Households", "Median Income")
  
  ward_profile <- expand.grid(
    `Ward Number` = ward_numbers,
    Year = census_years,
    Demographic = demographics
  ) |>
    mutate(
      Value = case_when(
        Demographic == "Population" ~ sample(500:5000, n(), replace = TRUE),
        Demographic == "Median Age" ~ sample(30:50, n(), replace = TRUE),
        Demographic == "Households" ~ sample(200:2000, n(), replace = TRUE),
        Demographic == "Median Income" ~ sample(30000:100000, n(), replace = TRUE)
  )
)
  
  write_csv(ward_profile, "data/raw_data/Simulated_WardData.csv")
  

}


# Simulate budget data
simulate_budget_data <- function() {
  set.seed(404)
  
  ward_numbers <- c(paste0("Ward ", 1:25), "CW")
  years <- 2022:2031
  programs <- c("Infrastructure", "Education", "Health", "Public Safety", "Recreation")
  
  budget_data <- expand.grid(
    `Ward Number` = ward_numbers,
    Year = years,
    Program = programs
  ) |>
    mutate(
      `Budget Item` = paste(Program, "Investment"),
      `Amount` = round(runif(n(), min = 50000, max = 500000), 2)
    )
  
  budget_data$Amount[sample(1:nrow(budget_data), 20)] <- NA
  budget_data$Amount[sample(1:nrow(budget_data), 10)] <- "-"
  
  budget_data_clean <- budget_data |>
    rename(
      `Program/Agency Name` = Program,
      `Capital Investment` = Amount
    )
  
  write_csv(budget_data_clean, "data/raw_data/Simulated_BudgetData.csv")
}


# Run simulation functions 
simulate_ward_profile()
simulate_budget_data()

