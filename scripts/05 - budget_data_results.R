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

### Data handling ###
# Import cleaned data 
budget_data_clean <- read_csv("data/analysis_data/2022-2031-budget_data.csv")

# Identify year columns
year_cols <- as.character(2022:2031)


# Reshape data to long format
budget_data_long <- budget_data_clean %>%
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "Year",
    values_to = "Funding"
  )


### Summing funding values ### 

# Total overall funding per ward number 
total_funding_ward <- budget_data_clean |>
  group_by(`Ward Number`) |>
  summarise(
    Total_Funding = sum(across(all_of(year_cols)), na.rm = TRUE)
  ) |>
  arrange(desc(Total_Funding))


# Funding for each year per ward number 
funding_yearly_ward <- budget_data_long |>
  group_by(`Ward Number`, Year) |>
  summarise(Funding = sum(Funding, na.rm = TRUE)) |>
  arrange(`Ward Number`, Year)


# Total funding per program/agency per ward number 
total_funding_program_ward <- budget_data_clean |>
  group_by(`Ward Number`, `Program/Agency Name`) |>
  summarise(Total_Funding = sum(across(all_of(year_cols)), na.rm = TRUE)) |>
  arrange(`Ward Number`, desc(Total_Funding))


  summarise(Total_Funding = sum(Funding, na.rm = TRUE)) |>
# Yearly funding per program/agency per ward number - maybe remove later
funding_yearly_program_ward <- budget_data_long |>
  group_by(`Ward Number`, `Program/Agency Name`, Year) |>
  summarise(
    Funding = sum(Funding, na.rm = TRUE)
  ) |>
  arrange(`Ward Number`, `Program/Agency Name`, Year)


### Tables and Visualizations ###
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


## Funding by ward ##

## Aggregate the data ##

## Total Overall Funding per Ward Number
total_funding_ward |>
  arrange(desc(Total_Funding)) |>
  kable(col.names = c("Ward Number", "Total Funding"), 
        format = "html", 
        caption = "Total Overall Funding per Ward Number") |>
  kable_styling(full_width = FALSE, position = "center")

# Total Overall Funding per Ward Number
ggplot(total_funding_ward, aes(x = reorder(`Ward Number`, -Total_Funding), y = Total_Funding)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Total Overall Funding per Ward Number",
    x = "Ward Number",
    y = "Total Funding"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 14)
  )


## Funding per Year per Ward Number
# Keep this will be useful


# Funding for Each Year per Ward Number
funding_yearly_ward |>
  pivot_wider(names_from = Year, values_from = Funding) |>
  arrange(`Ward Number`) |>
  kable(col.names = c("Ward Number", as.character(2022:2031)), 
        format = "html", 
        caption = "Yearly Funding per Ward Number") |>
  kable_styling(full_width = FALSE, position = "center")

# Funding Trends Over Years per Ward Number
ggplot(funding_yearly_ward, aes(x = as.integer(Year), y = Funding, color = `Ward Number`)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Funding Trends Over Years per Ward Number",
    x = "Year",
    y = "Funding",
    color = "Ward Number"
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

## Total funding per Program/Agency per Ward Number
# Will be important for any service and demographic level analysis if conducted


# Total Funding per Program/Agency Name for Each Ward Number
total_funding_program_ward |>
  arrange(`Ward Number`, desc(Total_Funding)) |>
  kable(col.names = c("Ward Number", "Program/Agency Name", "Total Funding"), 
        format = "html", 
        caption = "Total Funding per Program/Agency Name for Each Ward Number") |>
  kable_styling(full_width = FALSE, position = "center")



# Total Funding per Program/Agency Name for Each Ward Number
ggplot(total_funding_program_ward, aes(x = reorder(`Program/Agency Name`, -Total_Funding), y = Total_Funding, fill = `Program/Agency Name`)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Total Funding per Program/Agency Name for Each Ward Number",
    x = "Program/Agency Name",
    y = "Total Funding"
  ) +
  theme_minimal() +
  facet_wrap(~ `Ward Number`, scales = "free_y") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.title = element_text(size = 14),
    legend.position = "none"
  )

