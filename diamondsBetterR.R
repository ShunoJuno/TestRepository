# Diamonds Table of Descriptive Statistics ----

# Load packages
library(tidyverse)

# Load the diamonds dataset (included in ggplot2)
data(diamonds)

# Create summary table of carat statistics by color
diamonds_summary <- diamonds %>%
  group_by(color) %>%
  summarise(
    Count = n(),
    Min = min(carat, na.rm = TRUE),
    Quintile1 = quantile(carat, 0.20, na.rm = TRUE),
    Quintile2 = quantile(carat, 0.40, na.rm = TRUE),
    Median = median(carat, na.rm = TRUE),
    Quintile3 = quantile(carat, 0.60, na.rm = TRUE),
    Quintile4 = quantile(carat, 0.80, na.rm = TRUE),
    Max = max(carat, na.rm = TRUE),
    Mean = mean(carat, na.rm = TRUE),
    SD = sd(carat, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 3))
  ) %>%
  arrange(color)

# Render the table
diamonds_summary