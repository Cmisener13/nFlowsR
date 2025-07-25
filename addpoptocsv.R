#adds population figures to main csv
# Load required libraries
library(dplyr)
library(readr)

# Read the data
pop_df <- read_csv("CroplandArea.csv", locale = locale(encoding = "ISO-8859-1"))
original_df <- read_csv("Original_data_with_population.csv", locale = locale(encoding = "ISO-8859-1"))

# Check column names in pop_df
print(colnames(pop_df))

# Rename columns in pop_df to standard names if needed
pop_df <- pop_df %>%
  rename(
    Area = area,  # confirm this exists
    Year = Year,                  # confirm this exists
    cropland = Cropland                 # population values
  )

# Define country name mapping
country_name_map <- c(
  "Viet Nam" = "Vietnam",
  "China, Taiwan Province of" = "Taiwan",
  "Lao People's Democratic Republic" = "Laos",
  "Türkiye" = "Turkey",
  "China, mainland" = "China",
  "Brunei Darussalam" = "Brunei",
  "Yugoslav SFR" = "Yugoslavia",
  "Ethiopia PDR" = "Ethiopia",
  "Falkland Islands (Malvinas)" = "Falkland Islands",
  "China, Hong Kong SAR" = "Hong Kong",
  "United States of America" = "United States",
  "Republic of Korea" = "South Korea",
  "Democratic People's Republic of Korea" = "North Korea",
  "Syrian Arab Republic" = "Syria",
  "Iran (Islamic Republic of)" = "Iran",
  "Venezuela (Bolivarian Republic of)" = "Venezuela",
  "United Republic of Tanzania" = "Tanzania",
  "Republic of Moldova" = "Moldova",
  "Micronesia (Federated States of)" = "Micronesia",
  "Czechia" = "Czech Republic",
  "Côte d'Ivoire" = "Ivory Coast",
  "Eswatini" = "Swaziland",
  "Cabo Verde" = "Cape Verde"
)

# Apply the mapping
original_df <- original_df %>%
  mutate(Mapped_Area = recode(Area, !!!country_name_map))

# Check that column names are correct in both dataframes before merging
print(colnames(original_df))
print(colnames(pop_df))

# Merge population data
merged_df <- original_df %>%
  left_join(pop_df, by = c("Mapped_Area" = "Area", "Year.Code" = "Year")) %>%
  rename(CroplandArea = Value)

# Compute total population per GCAM region ID for each year
regional_pop_by_year <- merged_df %>%
  group_by(GCAM_region_ID, Year.Code) %>%
  summarise(GCAM_region_population_total = sum(Cropland, na.rm = TRUE), .groups = 'drop')

# Join total population back by region and year
merged_df <- merged_df %>%
  left_join(regional_pop_by_year, by = c("GCAM_region_ID", "Year.Code"))

# Save result
write_csv(merged_df, "Original_data_with_population_andCropland.csv")
