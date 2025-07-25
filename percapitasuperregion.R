#5 items per capita super region
# --- Load Required Libraries ---
library(ggplot2)
library(dplyr)
library(readr)
library(scales)

# --- File Paths ---
data_file <- "C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/Original_data_with_population.csv"
region_map_file <- "C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/GCAMregID.csv"
output_dir <- "C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/PerCapitaBySuperRegion"
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

# --- Super Region Groupings ---
region_groups <- list(
  "Africa" = c("Africa_Northern", "Africa_Eastern", "Africa_Southern", "Africa_Western", "South Africa"),
  "South America" = c("Colombia", "South America_Northern", "South America_Southern", "Brazil", "Argentina"),
  "North America + Central" = c("USA", "Canada", "Mexico", "Central America and Caribbean"),
  "Europe" = c("EU-12", "EU-15", "Europe_Non_EU", "Ukraine", "Russia", "European Free Trade Association"),
  "East Asia" = c("Japan", "Taiwan", "South Korea", "China"),
  "Southeast Asia + Oceania" = c("Southeast Asia", "Australia_NZ", "Indonesia"),
  "South Asia + Middle East" = c("India", "Central Asia", "Pakistan", "South Asia", "Middle East")
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
    Value = Value...4,           # Replace with correct column name if different
    Population = population      # Confirm column name
  )

# --- Join GCAM Region Mapping ---
region_map <- region_map %>%
  distinct(Area, .keep_all = TRUE)

data_joined <- data %>%
  left_join(region_map, by = "Area") %>%
  filter(!is.na(GCAM_region_ID))

# --- Normalize Per Capita and Filter ---
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

# --- Assign GCAM Region Name ---
normalized_data <- normalized_data %>%
  mutate(GCAM_Region_Name = gcam_names[as.character(GCAM_region_ID)]) %>%
  filter(!is.na(GCAM_Region_Name))

# --- Assign Super Region Name ---
region_lookup <- tibble(
  GCAM_Region_Name = unlist(region_groups),
  SuperRegion = rep(names(region_groups), times = lengths(region_groups))
)

normalized_data <- normalized_data %>%
  left_join(region_lookup, by = "GCAM_Region_Name") %>%
  filter(!is.na(SuperRegion))

# --- Plot per SuperRegion with Individual Region Lines ---
plot_data_all <- normalized_data %>%
  filter(SuperItem %in% names(item_groups))

for (super_region in unique(plot_data_all$SuperRegion)) {
  for (super_item in unique(plot_data_all$SuperItem)) {

    plot_data <- plot_data_all %>%
      filter(SuperRegion == super_region, SuperItem == super_item)

    if (nrow(plot_data) < 2) next

    p <- ggplot(plot_data, aes(x = Year.Code, y = Value_per_Capita, color = GCAM_Region_Name)) +
      geom_line(size = 1) +
      labs(
        title = paste(super_item, "in", super_region),
        x = "Year",
        y = "Per Capita Value",
        color = "Region"
      ) +
      scale_y_log10(labels = comma_format(), expand = c(0, 0)) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")

    filename <- paste0(
      gsub("[^A-Za-z0-9]", "_", super_region), "_",
      gsub("[^A-Za-z0-9]", "_", super_item), "_lines.png"
    )

    ggsave(
      filename = file.path(output_dir, filename),
      plot = p,
      width = 9, height = 6, dpi = 300
    )

    cat("✅ Saved:", filename, "\n")
  }
}

cat("🎉 All disaggregated super region graphs saved to:", output_dir, "\n")
