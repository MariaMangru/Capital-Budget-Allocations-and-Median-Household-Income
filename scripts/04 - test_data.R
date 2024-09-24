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

#### Load Data ####

# Cleaned ward data
ward_data_clean <- read_csv("../data/analysis_data/2023-WardProfile-Data.csv")

# Cleaned budget data
budget_data_clean <- read_csv("../data/analysis_data/2022-2031-budget_data.csv")

# Define year columns
year_cols <- as.character(2022:2031)

#### Tests ####

test_that("Funding Amounts are Numeric and Correctly Handled", {
  expect_true(all(sapply(ward_data_clean[year_cols], is.numeric)))
  expect_true(all(is.na(ward_data_clean[year_cols]) | ward_data_clean[year_cols] >= -Inf))
})


test_that("Negative Funding Amounts are Preserved", {
  negative_funding <- ward_data_clean |>
    filter(`Funding` < 0)
  
  expect_true(nrow(negative_funding) > 0)
})

test_that("Ward Numbers are Consistently Formatted", {
  expect_true(all(str_detect(ward_data_clean$`Ward Number`, "^Ward \\d+$")))
})

test_that("Program/Agency Names are Consistently Formatted", {
  expect_true(all(str_length(ward_data_clean$`Program/Agency Name`) > 0))
})


# Total Overall Funding per Ward Number
total_funding_ward_test <- ward_data_clean |>
  group_by(`Ward Number`) |>
  summarise(
    Total_Funding = sum(across(all_of(year_cols)), na.rm = TRUE)
  ) %>%
  arrange(desc(Total_Funding))

test_that("Total Funding per Ward Number is Correct", {
  sample_ward <- "Ward 1"
  expected_total <- ward_data_clean |>
    filter(`Ward Number` == sample_ward) |>
    summarise(
      expected = sum(across(all_of(year_cols)), na.rm = TRUE)
    ) |>
    pull(expected)
  
  actual_total <- total_funding_ward_test |>
    filter(`Ward Number` == sample_ward) |>
    pull(Total_Funding)
  
  expect_equal(actual_total, expected_total)
})

# Total Funding per Program/Agency per Ward Number
total_funding_program_ward_test <- ward_data_clean |>
  group_by(`Ward Number`, `Program/Agency Name`) |>
  summarise(
    Total_Funding = sum(across(all_of(year_cols)), na.rm = TRUE)
  ) |>
  arrange(`Ward Number`, desc(Total_Funding))

test_that("Total Funding per Program/Agency per Ward Number is Correct", {
  sample_ward <- "Ward 1"
  sample_program <- "Infrastructure"
  
  expected_total <- ward_data_clean |>
    filter(`Ward Number` == sample_ward, `Program/Agency Name` == sample_program) |>
    summarise(
      expected = sum(across(all_of(year_cols)), na.rm = TRUE)
    ) |>
    pull(expected)
  
  actual_total <- total_funding_program_ward_test |>
    filter(`Ward Number` == sample_ward, `Program/Agency Name` == sample_program) |>
    pull(Total_Funding)
  
  expect_equal(actual_total, expected_total)
})

#### Summary of Tests ####

# Run all tests
test_dir(".", reporter = "summary")