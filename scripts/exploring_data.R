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

budget_data_2022 <- read_csv("data/raw_data/2022-2031-capital-budget-and-plan-details.csv")

view(budget_data_2022)
colnames(budget_data_2022)
