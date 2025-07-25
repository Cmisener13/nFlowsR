#per capita USA western africa
# --- Load Required Libraries ---
library(ggplot2)
library(dplyr)
library(readr)
library(scales)

# --- Set File Paths ---
data_file <- "C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/Original_data_with_population.csv"
output_dir <- "C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/PerCapita_AvgCountryLine_USA_Africa_Western"
dir.create(output_dir, showWarnings = FALSE)

# --- Load Data ---
data <- read_csv(data_file, locale = locale(encoding = "latin1"))

# --- GCAM Region Mapping ---
gcam_names <- c(
  "1" = "USA", "2" = "Africa_Eastern", "3" = "Africa_Northern", "4" = "Africa_Southern",
  "5" = "Africa_Western", "6" = "Australia_NZ", "7" = "Brazil", "8" = "Canada",
  "9" = "Central America and Caribbean", "10" = "Central Asia", "11" = "China",
  "12" = "EU-12", "13" = "EU-15", "14" = "Ukraine", "15" = "Europe_Non_EU",
  "16" = "European Free Trade Association", "17" = "India", "18" = "Indonesia",
  "19" = "Japan", "20" = "Mexico", "21" = "Middle East", "22" = "Pakistan",
  "23" = "Russia", "24" = "South Africa", "25" = "South America_Northern",
  "26" = "South America_Southern", "27" = "South Asia", "28" = "South Korea",
  "29" = "Southeast Asia", "30" = "Taiwan", "31" = "Argentina", "32" = "Colombia"
)

# --- Add GCAM Region Names ---
data <- data %>%
  mutate(GCAM_regionID = as.character(GCAM_regionID),
         GCAM_region_name = gcam_names[GCAM_regionID])

# --- Livestock Groupings ---
item_groups <- list(
  "Swine" = c("Swine, breeding", "Swine, market"),
  "Poultry" = c("Chickens, broilers", "Chickens, layers", "Ducks", "Turkeys"),
  "Sheep & Goats" = c("Sheep", "Goats"),
  "Dairy" = c("Cattle, dairy"),
  "Beef" = c("Cattle, non-dairy")
)

# --- Filter and Normalize Data (Only USA and Africa_Western) ---
normalized_data <- data %>%
  filter(GCAM_region_name %in% c("USA", "Africa_Western"),
         Year.Code >= 1971, Year.Code <= 2021,
         !is.na(Value...4), !is.na(population)) %>%
  mutate(Value_per_Capita = Value...4 / population)

# --- Assign Super Item Category ---
normalized_data <- normalized_data %>%
  mutate(SuperItem = sapply(Item, function(x) {
    matched <- NA
    for (grp in names(item_groups)) {
      if (x %in% item_groups[[grp]]) {
        matched <- grp
        break
      }
    }
    if (is.na(matched)) x else matched
  }))

# --- Average total per capita values per GCAM region and year ---
region_avg <- normalized_data %>%
  group_by(GCAM_region_name, Year.Code) %>%
  summarise(AvgPerCapita = mean(Value_per_Capita, na.rm = TRUE), .groups = "drop")

# --- Plot both region lines on a single graph ---
p <- ggplot(region_avg, aes(x = Year.Code, y = AvgPerCapita, color = GCAM_region_name)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("USA" = "red3", "Africa_Western" = "lightblue2")) +
  scale_y_log10(labels = comma_format(), expand = c(0, 0)) +
  labs(
    title = "Average Per Capita Livestock Totals: USA vs. Africa_Western (1971-2021)",
    x = "Year",
    y = "Average Manure N per Capita (kg)",
    color = "Region"
  ) +
  theme_minimal(base_size = 14)

# --- Save the plot ---
ggsave(
  filename = file.path(output_dir, "AvgPerCapita_USA_vs_AfricaWestern.png"),
  plot = p,
  width = 10, height = 6, dpi = 300
)

cat("✅ Saved: AvgPerCapita_USA_vs_AfricaWestern.png\n")
