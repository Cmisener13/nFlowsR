library("tidyverse")
library(dplyr)
library(tidyr)

expanded_data <- read.csv("C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/2nd_fao_data.csv")

excluded_areas <- c(
  "Russian Federation", "Belarus", "Ukraine", "Estonia", "Latvia", "Lithuania",
  "Kazakhstan", "Turkmenistan", "Uzbekistan", "Kyrgyzstan", "Tajikistan",
  "Slovenia", "Croatia", "Czechoslovakia", "Slovakia", "Czechia", "Serbia",
  "Serbia and Montenegro", "North Macedonia", "Bosnia and Herzegovina",
  "Belgium-Luxembourg", "Eritrea", "Ethiopia", "Ethiopia PDR",
  "Falkland Islands (Malvinas)", "French Guiana", "Georgia", "Azerbaijan",
  "Armenia", "Greenland", "Faroe Islands", "Guadeloupe", "Guam",
  "Liechtenstein", "Luxembourg", "Martinique",
  "Micronesia (Federated States of)", "Mongolia", "Montserrat", "Palestine",
  "Republic of Moldova", "Réunion", "Saint Pierre and Miquelon",
  "Sudan", "South Sudan", "Sudan (former)",
  "United States Virgin Islands", "Wallis and Futuna Islands", "Western Sahara"
)

# Function to interpolate using approx()
interpolate_values <- function(df) {
  if (all(is.na(df$Value) | df$Value == 0)) return(df)
  valid <- df[!is.na(df$Value) & df$Value != 0, ]
  if (nrow(valid) < 2) return(df)  # Not enough data points to interpolate

  interp <- approx(x = valid$Year.Code, y = valid$Value, xout = df$Year.Code, rule = 2)
  df$Value[is.na(df$Value) | df$Value == 0] <- interp$y[is.na(df$Value) | df$Value == 0]
  return(df)
}

# Filter for areas NOT in excluded_areas and that have NA or 0 in Value
interpolation_candidates <- expanded_data %>%
  filter(!Area %in% excluded_areas & (is.na(Value) | Value == 0)) %>%
  select(Area, Item) %>%
  distinct()

# Apply interpolation by Area and Item
expanded_data <- expanded_data %>%
  group_by(Area, Item) %>%
  group_modify (~ if (any(interpolation_candidates$Area == .y$Area & interpolation_candidates$Item == .y$Item)) interpolate_values(.x) else .x) %>%
  ungroup()

# Save the updated CSV
write.csv(expanded_data, "3rd_fao_data.csv", row.names = FALSE)

cat("Interpolated data written to 3rd_fao_data.csv\n")
Iter.3 <- read_csv("3rd_fao_data.csv")
