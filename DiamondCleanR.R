# Diamonds Challenge - Clean Version

library(tidyverse)

# 1. Load and clean data ----
diamonds_clean <- diamonds %>%
  filter(x != 0, y != 0, z != 0) %>%   # remove invalid dimensions
  select(cut, x, y, z)

# 2. Create tidy summary table ----
diamond_stats <- diamonds_clean %>%
  pivot_longer(
    cols = c(x, y, z),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  
  group_by(cut, Variable) %>%
  summarise(
    Count  = n(),
    Min    = min(Value, na.rm = TRUE),
    Q1     = quantile(Value, 0.25, na.rm = TRUE),
    Median = median(Value, na.rm = TRUE),
    Q3     = quantile(Value, 0.75, na.rm = TRUE),
    Max    = max(Value, na.rm = TRUE),
    MAD    = mad(Value, na.rm = TRUE),
    Mean   = mean(Value, na.rm = TRUE),
    SD     = sd(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 2))
  ) %>%
  arrange(cut, Variable)

diamond_stats