#### Preamble ####
# Purpose: Tests the analysis datasets created 
# Author: Maria Mangru
# Date: September 2024
# Contact: maria.mangru@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(tidyverse)
library(testthat)
library(kableExtra)
library(scales)
library(readr)
library(here)


#### Load Cleaned Data ####
# Load cleaned budget data and ward profile data
budget_data_clean <- read_csv(here("data", "analysis_data", "clean_budget_data.csv"))
ward_data_clean <- read_csv(here("data", "analysis_data", "clean_ward_demo_data.csv"))

# Identify year columns
year_cols <- as.character(2022:2031)


### Tests ###

# Test that funding amounts are numeric and correctly handled 
test_that("Funding Amounts are Numeric and Correctly Handled", {
  # Check that all year columns are numeric
  expect_true(
    budget_data_clean %>%
      select(all_of(year_cols)) %>%
      summarise_all(is.numeric) %>%
      all()
  )
  
  expect_true(
    budget_data_clean %>%
      select(all_of(year_cols)) %>%
      mutate_all(is.finite) %>%
      all()
  )
})



# Test that Program/Agency names are non-empty strings 
test_that("Program/Agency Names are Non-Empty Strings", {
  expect_true(
    budget_data_clean %>%
      pull(`Program/Agency Name`) %>%
      str_trim() %>%
      nzchar() %>%
      all(),
    info = "All Program/Agency Names should be non-empty strings."
  )
})


# Test that total funding per Ward Number is correct 
test_that("Total Funding per Ward Number is Correct", {
  expected_totals <- budget_data_clean %>%
    group_by(`Ward Number`) %>%
    summarise(expected_total = sum(across(all_of(year_cols)), na.rm = TRUE))
  total_funding_ward_test <- budget_data_clean %>%
    group_by(`Ward Number`) %>%
    summarise(Total_Funding = sum(across(all_of(year_cols)), na.rm = TRUE)) %>%
    arrange(desc(Total_Funding))
  comparison <- expected_totals %>%
    inner_join(total_funding_ward_test, by = "Ward Number")
  expect_equal(comparison$Total_Funding, comparison$expected_total,
               info = "Total funding per Ward Number should match expected sums.")
})

# Test that total funding per Program/Agency per ward number is correct 
test_that("Total Funding per Program/Agency per Ward Number is Correct", {
  expected_totals <- budget_data_clean %>%
    group_by(`Ward Number`, `Program/Agency Name`) %>%
    summarise(expected_total = sum(across(all_of(year_cols)), na.rm = TRUE), .groups = 'drop')
  total_funding_program_ward_test <- budget_data_clean %>%
    group_by(`Ward Number`, `Program/Agency Name`) %>%
    summarise(Total_Funding = sum(across(all_of(year_cols)), na.rm = TRUE), .groups = 'drop') %>%
    arrange(`Ward Number`, desc(Total_Funding))
  comparison <- expected_totals %>%
    inner_join(total_funding_program_ward_test, by = c("Ward Number", "Program/Agency Name"))
  expect_equal(comparison$Total_Funding, comparison$expected_total,
               info = "Total funding per Program/Agency per Ward Number should match expected sums.")
})
