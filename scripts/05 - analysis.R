#### Preamble ####
# Purpose: Performs analysis of budget and merged demographic data
# Author: Maria Mangru
# Date: 27 September 2024
# Contact: maria.mangru@mail.utoronto.ca
# License: MIT

#### Workspace Setup ####
library(dplyr)
library(tidyr)
library(readr)
library(knitr)
library(kableExtra)
library(scales)
library(ggplot2)
library(stringr)
library(janitor)

#### Data Handling ####

# Define file paths
budget_file <- "data/analysis_data/clean_budget_data.csv"
demo_file <- "data/analysis_data/clean_ward_demo_data.csv"

# Read in the cleaned budget and demographic data
budget_data <- read_csv(budget_file)
demographic_data <- read_csv(demo_file)

# Identify year columns in budget data
year_cols <- as.character(2022:2031)

# Aggregate total funding per ward
total_funding_ward <- budget_data |>
  group_by(`Ward Number`) |>
  summarise(
    total_funding = sum(across(all_of(year_cols)), na.rm = TRUE)
  ) |>
  arrange(desc(total_funding)) |>
  mutate(total_funding = total_funding * 1000)  # Adjust funding to dollars

# Pivot demographic data from wide to long, then back to wide to ensure one row per ward
demographic_wide <- demographic_data |>
  pivot_longer(
    cols = -category,
    names_to = "Ward Number",
    values_to = "Value"
  ) |>
  pivot_wider(
    names_from = category,
    values_from = Value
  )

# Convert 'Ward Number' to numeric for merging
total_funding_ward <- total_funding_ward |>
  mutate(`Ward Number` = as.numeric(as.character(`Ward Number`)))

demographic_wide <- demographic_wide |>
  mutate(`Ward Number` = as.numeric(as.character(`Ward Number`)))

# Merge budget data with demographic data
merged_data <- total_funding_ward |>
  left_join(demographic_wide, by = "Ward Number") |>
  mutate(
    `Total Population` = as.numeric(gsub(",", "", `Total Population`)),
    per_capita_funding = total_funding / `Total Population`,
    `Bachelor’s degree or higher` = as.numeric(`Bachelor’s degree or higher`),
    `Total visible minority population` = as.numeric(`Total visible minority population`),
    `Bachelor’s degree or higher per capita` = `Bachelor’s degree or higher` / `Total Population`,
    `Total visible minority population per capita` = `Total visible minority population` / `Total Population`
  )

#### Tables and Visualizations ####

# Select relevant columns for the comprehensive table
merged_table <- merged_data |>
  select(
    `Ward Number`,
    total_funding,
    per_capita_funding,
    `Total Population`,
    `Median total income of households in 2020 ($)`,
    `Median total income of one-person households in 2020 ($)`,
    `Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)`,
    `Bachelor’s degree or higher per capita`,
    `Total visible minority population per capita`
  )

# Format numeric columns for presentation
merged_table_formatted <- merged_table |>
  mutate(
    total_funding = dollar(total_funding),
    per_capita_funding = dollar(per_capita_funding),
    `Total Population` = comma(`Total Population`),
    `Median total income of households in 2020 ($)` = dollar(as.numeric(`Median total income of households in 2020 ($)`)),
    `Median total income of one-person households in 2020 ($)` = dollar(as.numeric(`Median total income of one-person households in 2020 ($)`)),
    `Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)` = percent(as.numeric(`Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)`), accuracy = 0.01),
    `Bachelor’s degree or higher per capita` = percent(`Bachelor’s degree or higher per capita`, accuracy = 0.01),
    `Total visible minority population per capita` = percent(`Total visible minority population per capita`, accuracy = 0.01)
  )

# Create table
merged_table_formatted |>
  arrange(`Ward Number`) |>
  kable(
    col.names = c(
      "Ward Number", "Total Funding", "Per Capita Funding", "Total Population",
      "Household Median Income", "Individual Median Income",
      "Low Income Prevalence", "Bachelor’s Degree or Higher", "Visible Minority Population"
    ),
    format = "html",
    caption = "Total Funding and Demographic Data per Ward"
  ) |>
  kable_styling(full_width = FALSE, position = "center")

# Select and arrange specific columns for a secondary table
merged_table_select <- merged_data |>
  select(
    `Ward Number`,
    per_capita_funding,
    `Median total income of households in 2020 ($)`,
    `Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)`,
    `Bachelor’s degree or higher per capita`,
    `Total visible minority population per capita`
  ) |>
  arrange(desc(per_capita_funding))

# Format and apply conditional styling to the selected table
merged_table_2 <- merged_table_select |>
  mutate(
    per_capita_funding = dollar(per_capita_funding),
    `Median total income of households in 2020 ($)` = cell_spec(
      dollar(as.numeric(`Median total income of households in 2020 ($)`)),
      color = ifelse(as.numeric(`Median total income of households in 2020 ($)`) <= 84000, "red", "black"),
      bold = ifelse(as.numeric(`Median total income of households in 2020 ($)`) <= 84000, TRUE, FALSE)
    ),
    `Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)` = cell_spec(
      percent(as.numeric(`Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)`), accuracy = 0.01),
      color = ifelse(as.numeric(`Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)`) >= 0.13, "red", "black"),
      bold = ifelse(as.numeric(`Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)`) >= 0.13, TRUE, FALSE)
    ),
    `Bachelor’s degree or higher per capita` = percent(`Bachelor’s degree or higher per capita`, accuracy = 0.01),
    `Total visible minority population per capita` = percent(`Total visible minority population per capita`, accuracy = 0.01)
  )

# Create table
merged_table_2 |>
  kable(
    col.names = c(
      "Ward Number", "Per Capita Funding", "Household Median Income",
      "Low Income Prevalence", "Bachelor’s Degree or Higher", "Visible Minority Population"
    ),
    format = "html",
    escape = FALSE,
    caption = "Per Capita Funding and Selected Demographic Data per Ward"
  ) |>
  kable_styling(full_width = FALSE, position = "center")

#### Visualizations ####

# Wards by Per Capita Funding
ggplot(merged_data, aes(x = reorder(`Ward Number`, per_capita_funding), y = per_capita_funding)) +
  geom_bar(stat = "identity", fill = "#0072B2") +
  labs(
    title = "Wards by Per Capita Funding",
    x = "Ward Number",
    y = "Per Capita Funding ($CAD)"
  ) +
  scale_y_continuous(labels = dollar_format()) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.title = element_text(size = 14)
  )

# Per Capita Funding vs Median Household Income
ggplot(merged_data, aes(x = as.numeric(`Median total income of households in 2020 ($)`), y = per_capita_funding)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Per Capita Funding vs Median Household Income",
    x = "Median Household Income ($)",
    y = "Per Capita Funding ($)"
  ) +
  scale_x_continuous(labels = dollar_format()) +
  scale_y_continuous(labels = dollar_format()) +
  theme_minimal()

# Scatter Plot without Regression and with Ward 13 Removed
filtered_data <- merged_data |>
  filter(`Ward Number` != 13)

median_per_capita_funding <- median(filtered_data$per_capita_funding, na.rm = TRUE)

ggplot(filtered_data, aes(x = as.numeric(`Median total income of households in 2020 ($)`), y = per_capita_funding)) +
  geom_rect(
    aes(
      xmin = min(as.numeric(`Median total income of households in 2020 ($)`)),
      xmax = max(as.numeric(`Median total income of households in 2020 ($)`)),
      ymin = median_per_capita_funding,
      ymax = Inf
    ),
    fill = "lightblue",
    alpha = 0.3
  ) +
  geom_point() +
  geom_hline(yintercept = median_per_capita_funding, linetype = "dashed", color = "blue", size = 1) +
  labs(
    title = "Per Capita Funding vs Median Household Income",
    x = "Median Household Income ($)",
    y = "Per Capita Funding ($)"
  ) +
  scale_x_continuous(labels = dollar_format()) +
  scale_y_continuous(labels = dollar_format()) +
  theme_minimal()

#### Combined Bar and Line Chart for Top 5 and Bottom 5 Wards ####

# Select top 5 and bottom 5 wards by per capita funding
selected_wards <- merged_data |>
  arrange(desc(per_capita_funding)) |>
  slice_head(n = 5) |>
  bind_rows(
    merged_data |>
      arrange(per_capita_funding) |>
      slice_head(n = 5)
  ) |>
  mutate(Category = ifelse(`Ward Number` %in% slice_head(merged_data |>
                                                           arrange(desc(per_capita_funding)), n = 5)$`Ward Number`, "Top 5", "Bottom 5"))

# Ensure 'Prevalence of low income' is numeric
selected_wards <- selected_wards |>
  mutate(
    `Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)` = as.numeric(`Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)`)
  )

# Calculate scaling factor for dual-axis
scale_factor <- max(selected_wards$per_capita_funding, na.rm = TRUE) /
  max(selected_wards$`Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)`, na.rm = TRUE)

# Add scaled low income prevalence for plotting
selected_wards <- selected_wards |>
  mutate(
    Scaled_Low_Income_Prevalence = `Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)` * scale_factor
  )

# Create Combined Bar and Line Chart
ggplot(selected_wards, aes(x = reorder(`Ward Number`, per_capita_funding))) +
  geom_bar(aes(y = per_capita_funding, fill = Category), stat = "identity") +
  geom_line(aes(y = Scaled_Low_Income_Prevalence, group = 1, color = "Low Income Prevalence"), size = 1.2) +
  geom_point(aes(y = Scaled_Low_Income_Prevalence, color = "Low Income Prevalence"), size = 3) +
  scale_y_continuous(
    name = "Per Capita Funding ($CAD)",
    labels = dollar_format(),
    sec.axis = sec_axis(~ . / scale_factor, name = "Low Income Prevalence")
  ) +
  labs(
    title = "Top 5 and Bottom 5 Wards by Per Capita Funding with Low Income Prevalence",
    x = "Ward Number",
    fill = "Category",
    color = ""
  ) +
  scale_fill_manual(values = c("Top 5" = "#0072B2", "Bottom 5" = "red")) +
  scale_color_manual(values = c("Low Income Prevalence" = "black")) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.title.y.left = element_text(color = "black", size = 14),
    axis.title.y.right = element_text(color = "black", size = 14),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )