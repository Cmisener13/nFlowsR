#adds country to country code
# --- Load Required Libraries ---
library(readr)
library(dplyr)
library(countrycode)

# --- Load Your CSV File ---
df <- read_csv("C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/L100.Pop_thous_ctry_Yh.csv")

# --- Add Full Country Names from ISO Codes ---
df <- df %>%
  mutate(country_name = countrycode(iso, origin = "iso3c", destination = "country.name"))

# --- Preview ---
head(df)

# --- Optionally Save to a New File ---
write_csv(df, "C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/L100.Pop_with_names.csv")

cat("✅ Full country names added based on ISO3 codes.\n")
