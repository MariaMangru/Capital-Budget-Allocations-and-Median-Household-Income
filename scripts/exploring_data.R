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

# names of programs by wards only

ward_data <- read_csv("data/analysis_data/test.csv")

unique_programs <- ward_data |>
  select('Program/Agency Name') |>
  distinct()

unique_programs


## Exploring funding for each program type by year (2022 - 2031)

# Identify year columns
year_cols <- as.character(2022:2031)

clean_funding <- function(x) {
  x %>%
    str_replace_all("-", "0") %>%
    str_replace_all(",", "") %>%
    as.numeric()
}

# Apply the cleaning function to year columns
ward_data_clean <- ward_data %>%
  mutate(across(all_of(year_cols), clean_funding))

# Change data to Long Format
ward_data_long <- ward_data_clean %>%
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "Year",
    values_to = "Funding"
  )

# Aggregate funding by Program/Agency Name and Year
ward_funding <- ward_data_long %>%
  group_by(`Program/Agency Name`, Year) %>%
  summarise(Total_Funding = sum(Funding, na.rm = TRUE)) %>%
  ungroup()


# Visualize funding over years for each Program/Agency Name
ggplot(ward_funding, aes(x = as.integer(Year), y = Total_Funding, color = `Program/Agency Name`)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Funding Over Years by Program/Agency",
    x = "Year",
    y = "Total Funding",
    color = "Program/Agency Name"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = 2022:2031) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11)
  )


