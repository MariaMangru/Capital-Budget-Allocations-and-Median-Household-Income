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

### Data handling ###
# Import cleaned data 
budget_data_clean <- read_csv("data/analysis_data/2022-2031-budget_data.csv")

# Identify year columns
year_cols <- as.character(2022:2031)


# Reshape data to long format
budget_data_long <- budget_data_clean |>
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


# Yearly funding per program/agency per ward number - maybe remove later
funding_yearly_program_ward <- budget_data_long |>
  group_by(`Ward Number`, `Program/Agency Name`, Year) |>
  summarise(
    Funding = sum(Funding, na.rm = TRUE)
  ) |>
  arrange(`Ward Number`, `Program/Agency Name`, Year)


### Tables and Visualizations ###

## Tables ## 

# Total Overall Funding per Ward Number
total_funding_ward_table <- total_funding_ward |>
  arrange(desc(Total_Funding)) |>
  mutate(`Total Funding` = dollar(Total_Funding)) |>
  select(`Ward Number`, `Total Funding`)

total_funding_ward_table |>
  kable(
    col.names = c("Ward Number", "Total Funding"),
    format = "html",
    caption = "Total Overall Funding per Ward Number"
  ) |>
  kable_styling(full_width = FALSE, position = "center") |>
  column_spec(2, bold = TRUE, color = "white", background = "darkblue")


# Yearly Funding per Ward Number

funding_yearly_ward_table <- funding_yearly_ward |>
  pivot_wider(names_from = Year, values_from = Funding, values_fill = 0) %>%
  mutate_at(vars(all_of(year_cols)), ~ dollar(.)) |>
  arrange(`Ward Number`)

funding_yearly_ward_table|>
  kable(
    col.names = c("Ward Number", year_cols),
    format = "html",
    caption = "Yearly Funding for Wards"
  )|>
  kable_styling(full_width = TRUE, position = "center") |>
  scroll_box(width = "100%", height = "300px")

# Total Funding per Program/Agency Name for Each Ward Number
total_funding_program_ward_table <- total_funding_program_ward |>
  mutate(`Total Funding` = dollar(Total_Funding)) |>
  arrange(`Ward Number`, desc(Total_Funding)) |>
  select(`Ward Number`, `Program/Agency Name`, `Total Funding`)

total_funding_program_ward_table |>
  kable(
    col.names = c("Ward Number", "Program/Agency Name", "Total Funding"),
    caption = "Total Funding per Program/Agency Name for Each Ward Number"
  ) |>
  kable_styling(full_width = FALSE, position = "center") |>
  column_spec(3, bold = TRUE, color = "white", background = "darkgreen")

# Yearly Funding per Program/Agency Name
funding_yearly_program_ward_table <- funding_yearly_program_ward |>
  pivot_wider(names_from = Year, values_from = Funding, values_fill = 0) |>
  mutate_at(vars(all_of(year_cols)), ~ dollar(.)) |>
  arrange(`Ward Number`, `Program/Agency Name`)

funding_yearly_program_ward_table |>
  kable(
    col.names = c("Ward Number", "Program/Agency Name", year_cols),
    caption = "Yearly Funding per Program/Agency Name"
  ) |>
  kable_styling(full_width = TRUE, position = "center") |>
  scroll_box(width = "100%", height = "300px")

### Graphs ###

# Wards by Total Funding
ggplot(total_funding_ward, aes(x = reorder(`Ward Number`, Total_Funding), y = Total_Funding)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Wards by Total Funding",
    x = "Ward Number",
    y = "Total Funding"
  ) +
  scale_y_continuous(labels = dollar_format()) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.title = element_text(size = 14)
  )

# Funding Trends Over Years - needs lots of fixing :(
ggplot(funding_yearly_ward, aes(x = as.integer(Year), y = Funding, color = `Ward Number`)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Funding Trends Over Years",
    x = "Year",
    y = "Funding",
    #color = "Ward Number"
  ) +
  scale_x_continuous(breaks = 2022:2031) +
  scale_y_continuous(labels = dollar_format()) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11)
  )