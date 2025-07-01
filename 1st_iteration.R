library("tidyverse")
library(dplyr)
library(tidyr)

# Load the CSV file
data <- read.csv("C:/Users/mise017/Downloads/FAO_excel_(6-30-25)_1-58PM (1).csv")

# Ensure the year column is numeric if it isn't already
data$Year.Code <- as.numeric(as.character(data$Year.Code))

# Define full year range
years_full <- 1961:2022

# Assume "area" and "item" are the correct grouping columns
# Adjust if the actual column names are different
expanded_data <- data %>%
  filter(!is.na(Value) & Value != 0) %>%
  distinct(Area, Item) %>%
  crossing(Year.Code = years_full) %>%
  left_join(data, by = c("Area", "Item", "Year.Code")) %>%
  arrange(Area, Item, Year.Code)

# Write to a new CSV file
write.csv(expanded_data, "expanded_fao_data.csv", row.names = FALSE)

cat("Expanded data written to expanded_fao_data.csv\n")
