#### Preamble ####
# Purpose: Performs analysis of budget and merged data
# Author: Maria Mangru
# Date: 27 September 2024
# Contact: maria.mangru@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
library(dplyr)
library(tidyr)
library(readr)
library(knitr)
library(kableExtra)
library(scales)
library(ggplot2)
library(stringr)
library(janitor)


### Data Handling ###

# Read in the cleaned budget data
budget_data <- read_csv("data/analysis_data/clean_budget_data.csv")

# Read in the cleaned demographic data
demographic_data <- read_csv("data/analysis_data/clean_ward_demo_data.csv")

# Identify year columns in budget data
year_cols <- as.character(2022:2031)

# Aggregate total funding per ward number
total_funding_ward <- budget_data |>
  group_by(`Ward Number`)|>
  summarise(
    total_funding = sum(across(all_of(year_cols)), na.rm = TRUE)
  ) |>
  arrange(desc(total_funding))

# Adjust total funding (amounts are in thousands of dollars)
total_funding_ward <- total_funding_ward |>
  mutate(total_funding = total_funding * 1000)

# Pivot demographic data from wide to long format
demographic_long <- demographic_data |>
  pivot_longer(
    cols = -category,
    names_to = "Ward Number",
    values_to = "Value"
  )

# Pivot demographic data back to wide format to get one row per ward
demographic_wide <- demographic_long |>
  pivot_wider(
    names_from = category,
    values_from = Value
  )

# Convert 'Ward Number' to numeric in both datasets to allow merging
total_funding_ward$`Ward Number` <- as.numeric(as.character(total_funding_ward$`Ward Number`))
demographic_wide$`Ward Number` <- as.numeric(as.character(demographic_wide$`Ward Number`))

# Merge budget data with demographic data
merged_data <- total_funding_ward |>
  left_join(demographic_wide, by = "Ward Number")

# Convert 'Total Population' to numeric
merged_data$`Total Population` <- as.numeric(gsub(",", "", merged_data$`Total Population`))


# Create per capita funding variable
merged_data <- merged_data |>
  mutate(per_capita_funding = total_funding / `Total Population`)


# Convert columns from list to double 
merged_data$`Bachelor’s degree or higher` <- as.numeric(merged_data$`Bachelor’s degree or higher`)
merged_data$`Total visible minority population` <- as.numeric(merged_data$`Total visible minority population`)


#Create per capita bachelor's degree variable
merged_data <- merged_data |>
  mutate(`Bachelor’s degree or higher per capita` = `Bachelor’s degree or higher` / `Total Population`)


#Create per capita visible minority population variable
merged_data <- merged_data |>
  mutate(`Total visible minority population per capita` = `Total visible minority population` / `Total Population`)


### Tables and Visualizations ###

# Select relevant columns for the table
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


# Format numeric columns
merged_table_formatted <- merged_table |>
  mutate(
    total_funding = dollar(total_funding),
    per_capita_funding = dollar(per_capita_funding),
    `Total Population` = comma(`Total Population`),
    `Median total income of households in 2020 ($)` = dollar(as.numeric(`Median total income of households in 2020 ($)`)),
    `Median total income of one-person households in 2020 ($)` = dollar(as.numeric(`Median total income of one-person households in 2020 ($)`)),
    `Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)` = paste0(round(as.numeric(`Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)`) * 100, 2), "%"),
    `Bachelor’s degree or higher per capita` = paste0(round(as.numeric(`Bachelor’s degree or higher per capita`) * 100, 2), "%"),
    `Total visible minority population per capita` = paste0(round(as.numeric(`Total visible minority population per capita`) * 100, 2), "%")
  )


# Create a table for the merged data funding and ALL demographic data

merged_table_formatted |>
  arrange(`Ward Number`)|>
  kable(
    col.names = c("Ward Number", "Total Funding", "Per Capita Funding", "Total Population", "Household Median Income", "Individual Median Income","Low Income Prevalence", "Bachelor’s degree or higher", "Visible minority population"),
    format = "html",
    caption = "Total Funding and Demographic Data per Ward"
  ) |>
  kable_styling(full_width = FALSE, position = "center")



# Table creation for only a select few variables 
# This will be used in the quarto! - need to update the low income prevalence 

merged_table_select <- merged_data |>
  select(
    `Ward Number`,
    per_capita_funding,
    `Median total income of households in 2020 ($)`,
    `Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)`,
    `Bachelor’s degree or higher per capita`,
    `Total visible minority population per capita`
  )

# Arrange by the numeric version of per capita funding before formatting
merged_table_select <- merged_table_select |>
  arrange(desc(per_capita_funding))

# Format numeric columns
merged_table_2 <- merged_table_select |>
  mutate(
    per_capita_funding = dollar(per_capita_funding),
    
    # Apply conditional formatting to highlight Household Median Income <= 84000
    `Median total income of households in 2020 ($)` = cell_spec(
      dollar(as.numeric(`Median total income of households in 2020 ($)`)),
      color = ifelse(as.numeric(`Median total income of households in 2020 ($)`) <= 84000, "red", "black"),
      bold = ifelse(as.numeric(`Median total income of households in 2020 ($)`) <= 84000, TRUE, FALSE)
    ),
    
    # Apply conditional formatting to highlight Low Income Prevalence >= 13%
    `Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)` = cell_spec(
      paste0(round(as.numeric(`Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)`) * 100, 2), "%"),
      color = ifelse(as.numeric(`Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)`) >= 13, "red", "black"),
      bold = ifelse(as.numeric(`Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)`) >= 13, TRUE, FALSE)
    ),
    
    # Convert the remaining numeric columns to formatted strings
    `Bachelor’s degree or higher per capita` = paste0(round(as.numeric(`Bachelor’s degree or higher per capita`) * 100, 2), "%"),
    `Total visible minority population per capita` = paste0(round(as.numeric(`Total visible minority population per capita`) * 100, 2), "%")
  )

# Create a table for the merged data funding and all demographic data
merged_table_2 |>
  kable(
    col.names = c("Ward Number", "Per Capita Funding", "Household Median Income", "Low Income Prevalence", "Bachelor’s degree or higher", "Visible minority population"),
    format = "html",
    escape = FALSE, 
    caption = "Total Funding and Demographic Data per Ward"
  ) |>
  kable_styling(full_width = FALSE, position = "center")


# Visualizations

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

### Scatter plots for per capita funding and median household income ###


# Scatter plot of Per Capita Funding vs Median Household Income
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


# Scatter plot of Per Capita Funding vs Median Household Income with ward 13 removed 

filtered_data <- merged_data |>
  filter(`Ward Number` != 13)

ggplot(filtered_data, aes(x = as.numeric(`Median total income of households in 2020 ($)`), y = per_capita_funding)) +
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



# without regression 
filtered_data <- merged_data |>
  filter(`Ward Number` != 13)

median_per_capita_funding <- median(filtered_data$per_capita_funding, na.rm = TRUE)

ggplot(filtered_data, aes(x = as.numeric(`Median total income of households in 2020 ($)`), y = per_capita_funding)) +
  geom_rect(aes(xmin = min(as.numeric(`Median total income of households in 2020 ($)`)), 
                xmax = max(as.numeric(`Median total income of households in 2020 ($)`)),
                ymin = median_per_capita_funding, ymax = Inf),
            fill = "lightblue", alpha = 0.3) +
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


# Save merged data to CSV
write_csv(merged_data, "data/analysis_data/merged_data.csv")






### Needs cleaning to better integrate with the rest of the script 


# Select top 5 and bottom 5 wards by per capita funding 
top5_wards <- merged_data |>
  arrange(desc(per_capita_funding)) |>
  slice(2:6)

bottom5_wards <- merged_data |>
  arrange(per_capita_funding)|>
  slice(1:5)

# Combine these wards 
selected_wards <- bind_rows(top5_wards, bottom5_wards) |>
  mutate(Category = ifelse(`Ward Number` %in% top5_wards$`Ward Number`, "Top 5", "Bottom 5"))

selected_wards$`Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)` <- as.numeric(as.character(selected_wards$`Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)`))


# Calculate scaling factor
max_funding <- max(selected_wards$per_capita_funding, na.rm = TRUE)
max_low_income <- max(selected_wards$`Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)`, na.rm = TRUE)
scale_factor <- max_funding / max_low_income

# Add a scaled Low Income Prevalence for plotting
selected_wards <- selected_wards |>
  mutate(Scaled_Low_Income_Prevalence = `Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)` * scale_factor)


# Create the Combined Bar and Line Chart
ggplot(selected_wards, aes(x = reorder(`Ward Number`, per_capita_funding))) +
  geom_bar(aes(y = per_capita_funding, fill = Category), stat = "identity") +
  geom_line(aes(y = Scaled_Low_Income_Prevalence, group = 1, color = "Low Income Prevalence"), size = 1.2) +
  geom_point(aes(y = Scaled_Low_Income_Prevalence, color = "Low Income Prevalence"), size = 3) +
 
   scale_y_continuous(
    name = "Per Capita Funding ($CAD)",
    labels = dollar_format(),
    sec.axis = sec_axis(~./scale_factor, name = "Low Income Prevalence")
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




