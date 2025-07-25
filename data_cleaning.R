#expand the rows
library("tidyverse")
library(dplyr)
library(tidyr)

# Load the CSV file
data <- read.csv("C:/Users/mise017/Downloads/FAO_excel_(6-30-25)_1-58PM (1).csv")
# head(data)
# data$year
# data$Year.Code
# Ensure the year column is numeric if it isn't already
data$year <- as.numeric(as.character(data$Year.Code))
# head(data)
# Identify the complete range of years
years_full <- 1961:2022

# Group by livestock category and apply the logic
expanded_data <- data %>%
  group_by(Item) %>%
  group_modify(~ {
    df <- .x
    has_non_zero <- any(df$value != 0, na.rm = TRUE)
    has_missing_years <- length(intersect(df$year, years_full)) < length(years_full)

    if (has_non_zero && has_missing_years) {
      complete(df, year = years_full, nesting(across(-year)), fill = list(value = NA))
    } else {
      df
    }
  }) %>%
  ungroup()

head(expanded_data)
expanded_data_new <- add_row(expanded_data, Value = NA, .before = 2)


# Write to a new CSV file
write.csv(expanded_data, "expanded_fao_data.csv", row.names = FALSE)

cat("Expanded data written to expanded_fao_data.csv\n")


getwd()
normalizePath("expanded_fao_data.csv")
file.exists("expanded_fao_data.csv")
