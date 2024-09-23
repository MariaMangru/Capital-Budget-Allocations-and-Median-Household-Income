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

# names of programs by wards only

ward_data <- read_csv("data/analysis_data/test.csv")

unique_programs <- ward_data |>
  select('Program/Agency Name') |>
  distinct()

unique_programs




# Identify year columns
year_cols <- as.character(2022:2031)

clean_funding <- function(x) {
  x |>
    str_replace_all("-", "0") |>
    str_replace_all(",", "") |>
    as.numeric()
}

# Apply the cleaning function to year columns
ward_data_clean <- ward_data |>
  mutate(across(all_of(year_cols), clean_funding))

# Change data to Long Format
ward_data_long <- ward_data_clean |>
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "Year",
    values_to = "Funding"
  )

## Exploring funding for each program type by year (2022 - 2031) ##

# Aggregate funding by Program/Agency Name and Year
ward_funding <- ward_data_long |>
  group_by(`Program/Agency Name`, Year) |>
  summarise(Total_Funding = sum(Funding, na.rm = TRUE)) |>
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


## Funding by ward ##

## Aggregate the data ##

# Total overall funding per Ward Number
# NOTE: numbers aren't correct but this will still be important so keep 
total_funding_ward <- ward_data_clean |>
  group_by(`Ward Number`) |>
  summarise(Total_Funding = sum(`2022`:`2031`, na.rm = TRUE)) |>
  arrange(desc(Total_Funding))

## Total Overall Funding per Ward Number
total_funding_ward |>
  arrange(desc(Total_Funding)) |>
  kable(col.names = c("Ward Number", "Total Funding"), 
        format = "html", 
        caption = "Total Overall Funding per Ward Number") |>
  kable_styling(full_width = FALSE, position = "center")



## Funding per Year per Ward Number
# Keep this will be useful
funding_yearly_ward <- ward_data_long |>
  group_by(`Ward Number`, Year) |>
  summarise(Funding = sum(Funding, na.rm = TRUE)) |>
  arrange(`Ward Number`, Year)

# Funding for Each Year per Ward Number
funding_yearly_ward |>
  pivot_wider(names_from = Year, values_from = Funding) |>
  arrange(`Ward Number`) |>
  kable(col.names = c("Ward Number", as.character(2022:2031)), 
        format = "html", 
        caption = "Yearly Funding per Ward Number") |>
  kable_styling(full_width = FALSE, position = "center")



## Total funding per Program/Agency per Ward Number
# Keep because it will be important for any service and demographic level analysis
total_funding_program_ward <- ward_data_clean |>
  group_by(`Ward Number`, `Program/Agency Name`) |>
  summarise(Total_Funding = sum(`2022`:`2031`, na.rm = TRUE)) |>
  arrange(`Ward Number`, desc(Total_Funding))

# Total Funding per Program/Agency Name for Each Ward Number
total_funding_program_ward |>
  arrange(`Ward Number`, desc(Total_Funding)) |>
  kable(col.names = c("Ward Number", "Program/Agency Name", "Total Funding"), 
        format = "html", 
        caption = "Total Funding per Program/Agency Name for Each Ward Number") |>
  kable_styling(full_width = FALSE, position = "center")




## Yearly funding per Program/Agency per Ward Number
# Keep for now but might be too detailed 
funding_yearly_program_ward <- ward_data_long |>
  group_by(`Ward Number`, `Program/Agency Name`, Year) |>
  summarise(Funding = sum(Funding, na.rm = TRUE)) |>
  arrange(`Ward Number`, `Program/Agency Name`, Year)

# Yearly Funding per Program/Agency Name for Each Ward Number
funding_yearly_program_ward |>
  arrange(`Ward Number`, `Program/Agency Name`, Year) |>
  pivot_wider(names_from = Year, values_from = Funding) |>
  kable(col.names = c("Ward Number", "Program/Agency Name", as.character(2022:2031)), 
        format = "html", 
        caption = "Yearly Funding per Program/Agency Name for Each Ward Number") |>
  kable_styling(full_width = FALSE, position = "center")


## Graphs ##

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

