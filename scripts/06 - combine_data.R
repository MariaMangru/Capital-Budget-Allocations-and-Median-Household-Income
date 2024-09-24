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


### Load Data ###
budget_data_clean <- read_csv("data/analysis_data/2022-2031-budget_data.csv")
ward_data_clean <- read_csv("data/analysis_data/2023-WardProfile-Data.csv")


### Combine Data ##
# Rename the first column to 'Category' for clarity
ward_profile_clean <- ward_data_clean |>
  rename(Category = `...1`) |> 
  select(-Toronto) |>            
  pivot_longer(
    cols = starts_with("Ward"),
    names_to = "Ward Number",
    values_to = "Value"
  ) |>
  pivot_wider(
    names_from = Category,
    values_from = Value
  )

View(ward_profile_clean)

# Identify year columns
year_cols <- as.character(2022:2031)

# Aggregate total funding per ward
total_funding_ward <- budget_data_clean %>%
  group_by(`Ward Number`) %>%
  summarise(
    Total_Funding = sum(across(all_of(year_cols)), na.rm = TRUE)
  ) %>%
  arrange(desc(Total_Funding))

# Merge total_funding_ward with ward_data_clean
combined_data <- total_funding_ward %>%
  left_join(ward_data_clean, by = "Ward Number")


# Calculate funding per capita
combined_data <- combined_data %>%
  mutate(
    Funding_Per_Capita = Total_Funding / `Total Population`
  )

# Calculate funding as a percentage of median income
combined_data <- combined_data %>%
  mutate(
    Funding_Per_Median_Income = Total_Funding / `Median total income of households in 2020 ($)`
  )


### Tables ### 
# Combined Funding and Demographics Table
combined_table <- combined_data %>%
  select(
    `Ward Number`,
    Total_Funding,
    `Total Population`,
    `Average total income of households in 2020 ($)`,
    Funding_Per_Capita,
    Funding_Per_Median_Income
  ) %>%
  mutate(
    Total_Funding = dollar(Total_Funding),
    `Average Total Income` = dollar(`Average total income of households in 2020 ($)`),
    Funding_Per_Capita = dollar(Funding_Per_Capita),
    Funding_Per_Median_Income = dollar(Funding_Per_Median_Income)
  )

combined_table %>%
  kable(
    col.names = c("Ward Number", "Total Funding", "Total Population", "Average Household Income (2020)", 
                  "Funding Per Capita", "Funding Per Median Income"),
    format = "html",
    caption = "Combined Funding and Demographic Data per Ward"
  ) %>%
  kable_styling(full_width = FALSE, position = "center") %>%
  column_spec(2, bold = TRUE, color = "white", background = "darkblue") %>%
  column_spec(5:6, background = "lightgreen")


### Visualizations ###

# Funding vs Population
ggplot(combined_data, aes(x = `Total Population`, y = Total_Funding)) +
  geom_point(color = "steelblue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  labs(
    title = "Total Funding vs. Population per Ward",
    x = "Total Population",
    y = "Total Funding"
  ) +
  scale_y_continuous(labels = dollar_format()) +
  scale_x_continuous(labels = comma) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

# Funding Per Capita Across Wards
ggplot(combined_data, aes(x = reorder(`Ward Number`, Funding_Per_Capita), y = Funding_Per_Capita)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(
    title = "Funding Per Capita Across Wards",
    x = "Ward Number",
    y = "Funding Per Capita"
  ) +
  scale_y_continuous(labels = dollar_format()) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.title = element_text(size = 14)
  )

# Funding vs Median Income
ggplot(combined_data, aes(x = `Median total income of households in 2020 ($)`, y = Total_Funding)) +
  geom_point(color = "purple", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "orange") +
  labs(
    title = "Total Funding vs. Median Household Income per Ward",
    x = "Median Household Income (2020)",
    y = "Total Funding"
  ) +
  scale_y_continuous(labels = dollar_format()) +
  scale_x_continuous(labels = dollar_format()) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )



                              