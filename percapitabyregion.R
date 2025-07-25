#5 items normalized for population by region draft 3 (ts works)
# --- Load Required Libraries ---
library(ggplot2)
library(dplyr)
library(readr)
library(scales)

# --- File Paths ---
data_file <- "C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/Original_data_with_population.csv"
region_map_file <- "C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/GCAMregID.csv"
output_dir <- "C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/PerCapitaByGCAMRegion"
dir.create(output_dir, showWarnings = FALSE)

# --- Load Data ---
data <- read_csv(data_file, locale = locale(encoding = "latin1"))
region_map <- read_csv(region_map_file)

# --- GCAM Region Name Mapping ---
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

# --- Livestock Groupings ---
item_groups <- list(
  "Swine" = c("Swine, breeding", "Swine, market"),
  "Poultry" = c("Chickens, broilers", "Chickens, layers", "Ducks", "Turkeys"),
  "Sheep & Goats" = c("Sheep", "Goats"),
  "Dairy" = c("Cattle, dairy"),
  "Beef" = c("Cattle, non-dairy")
)

# --- Clean and Rename Columns ---
data <- data %>%
  rename(
    Value = Value...4,           # Replace with actual column name if needed
    Population = population   # Make sure this is lowercase
  )

# --- Join GCAM Region Mapping ---
region_map <- region_map %>%
  distinct(Area, .keep_all = TRUE)

data_joined <- data %>%
  left_join(region_map, by = "Area") %>%
  filter(!is.na(GCAM_region_ID))

# --- Filter and Normalize ---
normalized_data <- data_joined %>%
  filter(Year.Code >= 1971, Year.Code <= 2021) %>%
  filter(!is.na(Value), !is.na(Population)) %>%
  mutate(Value_per_Capita = Value / Population)

# --- Assign Super Item Category ---
normalized_data <- normalized_data %>%
  mutate(
    SuperItem = case_when(
      Item %in% item_groups[["Swine"]] ~ "Swine",
      Item %in% item_groups[["Poultry"]] ~ "Poultry",
      Item %in% item_groups[["Sheep & Goats"]] ~ "Sheep & Goats",
      Item %in% item_groups[["Dairy"]] ~ "Dairy",
      Item %in% item_groups[["Beef"]] ~ "Beef",
      TRUE ~ Item
    )
  )

# --- Add GCAM Region Name from Mapping ---
normalized_data <- normalized_data %>%
  mutate(
    GCAM_Region_Name = gcam_names[as.character(GCAM_region_ID)]
  ) %>%
  filter(!is.na(GCAM_Region_Name))  # remove unmatched IDs

# --- Aggregate Data by GCAM Region ---
aggregated_data <- normalized_data %>%
  group_by(GCAM_Region_Name, SuperItem, Year.Code) %>%
  summarise(Value = sum(Value_per_Capita, na.rm = TRUE), .groups = "drop")

# --- Generate Graphs ---
for (this_region in unique(aggregated_data$GCAM_Region_Name)) {
  for (super_item in unique(aggregated_data$SuperItem)) {

    plot_data <- aggregated_data %>%
      filter(GCAM_Region_Name == this_region, SuperItem == super_item)

    if (nrow(plot_data) < 2) next  # Skip empty plots

    p <- ggplot(plot_data, aes(x = Year.Code, y = Value)) +
      geom_line(color = "steelblue", size = 1) +
      labs(
        title = paste(super_item, "in", this_region),
        x = "Year",
        y = "Per Capita Value"
      ) +
      scale_y_log10(labels = comma_format(), expand = c(0, 0)) +
      theme_minimal(base_size = 14)

    filename <- paste0(
      gsub("[^A-Za-z0-9]", "_", this_region), "_",
      gsub("[^A-Za-z0-9]", "_", super_item),
      ".png"
    )

    ggsave(
      filename = file.path(output_dir, filename),
      plot = p,
      width = 8, height = 5, dpi = 300
    )

    cat("✅ Saved:", filename, "\n")
  }
}

cat("🎉 All GCAM region graphs saved to:", output_dir, "\n")
