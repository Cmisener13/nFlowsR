#uses the approx.fun function for the smaller territories
library(tidyverse)
library(dplyr)
library(tidyr)

expanded_data <- read.csv("C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/2nd_fao_data.csv")

excluded_areas <- c(
  "Russian Federation", "Belarus", "Ukraine", "Estonia", "Latvia", "Lithuania",
  "Kazakhstan", "Turkmenistan", "Uzbekistan", "Kyrgyzstan", "Tajikistan",
  "Slovenia", "Croatia", "Czechoslovakia", "Slovakia", "Czechia", "Serbia",
  "Serbia and Montenegro", "North Macedonia", "Bosnia and Herzegovina",
  "Belgium-Luxembourg", "Eritrea", "Ethiopia", "Ethiopia PDR", "Georgia", "Azerbaijan",
  "Armenia", "Luxembourg", "Republic of Moldova", "Sudan", "South Sudan", "Sudan (former)"
)

expanded_data$Value <- as.numeric(expanded_data$Value)
expanded_data$Year.Code <- as.numeric(expanded_data$Year.Code)

# Interpolate NA or 0 values based on linear regression per Area/Item combo
interpolated_data <- expanded_data %>%
  group_by(Area, Item) %>%
  mutate(Value = {
    area_name <- unique(Area)
    if (area_name %in% excluded_areas) {
      Value
    } else {
      x <- Year.Code
      y <- Value
      valid <- which(!is.na(y) & y != 0)
      if (length(valid) >= 2) {
        fit <- lm(y ~ x, data = data.frame(x = x[valid], y = y[valid]))
        predicted <- predict(fit, newdata = data.frame(x = x))
        y[is.na(y) | y == 0] <- predicted[is.na(y) | y == 0]
      }
      y
    }
  }) %>%
  ungroup()

# Save the updated CSV
write.csv(expanded_data, "4th_fao_data.csv", row.names = FALSE)

cat("Interpolated data written to 4th_fao_data.csv\n")
Iter.4 <- read_csv("4th_fao_data.csv")
