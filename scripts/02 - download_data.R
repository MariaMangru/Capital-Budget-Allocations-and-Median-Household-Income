#### Preamble ####
# Purpose: Downloads data from Open Data Toronto 
# Author: Maria Mangru
# Date: September 2024
# Contact: maria.mangru@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(opendatatoronto)
library(dplyr)
library(readr)

#### Download data ####

## Ward Profile Data ##
ward_profile <- show_package("6678e1a6-d25f-4dff-b2b7-aa8f042bc2eb")
ward_profile

## 2022 - 2031 Budget Data ##
raw_budget_data <- show_package("7d3bcf2f-8eca-4ed5-a352-a34adb130931")
raw_budget_data

#### Save data ####
write_csv(ward_profile, "data/raw_data/WardData.csv") 
write_csv(ward_profile, "data/raw_data/BudgetData.csv") 

         
