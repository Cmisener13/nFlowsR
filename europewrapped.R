#5 superregions fact wrapped just for europe
#5 items, super regions, face_wrap
# --- Load Required Libraries ---
library(ggplot2)
library(dplyr)
library(readr)
library(scales)

# --- Set File Paths ---
data_file <- "C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/Original_data_GCAMreg.csv"
output_dir <- "C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/EuropeSuperRegionGraphs"
dir.create(output_dir, showWarnings = FALSE)

# --- Load Data ---
data <- read_csv(data_file, locale = locale(encoding = "latin1"))

# --- GCAM Region Name Mapping ---
gcam_names <- c(
  "1" = "USA", "2" = "Africa_Eastern", "3" = "Africa_Northern", "4" = "Africa_Southern",
  "5" = "Africa_Western", "6" = "Australia_NZ", "7" = "Brazil", "8" = "Canada",
  "9" = "Central America and Caribbean", "10" = "Central Asia", "11" = "China",
  "12" = "Eastern_Europe", "13" = "Western_Europe", "14" = "Ukraine", "15" = "Europe_Non_EU",
  "16" = "European Free Trade Association", "17" = "India", "18" = "Indonesia",
  "19" = "Japan", "20" = "Mexico", "21" = "Middle East", "22" = "Pakistan",
  "23" = "Russia", "24" = "South Africa", "25" = "South America_Northern",
  "26" = "South America_Southern", "27" = "South Asia", "28" = "South Korea",
  "29" = "Southeast Asia", "30" = "Taiwan", "31" = "Argentina", "32" = "Colombia"
)

# --- Region Groupings ---
region_groups <- list(
  "Europe" = c("Eastern_Europe", "Western_Europe", "Ukraine", "Russia")
)

# --- Livestock Groupings ---
item_groups <- list(
  "Swine" = c("Swine, breeding", "Swine, market"),
  "Poultry" = c("Chickens, broilers", "Chickens, layers", "Ducks", "Turkeys"),
  "Sheep & Goats" = c("Sheep", "Goats"),
  "Beef" = c("Cattle, non-dairy"),
  "Dairy" = c("Cattle, dairy")
)

# --- Prepare Data ---
filtered_data <- data %>%
  filter(!is.na(GCAMreg), GCAMreg %in% 1:32, Year.Code >= 1961, Year.Code <= 2022) %>%
  mutate(GCAM_Region_Name = gcam_names[as.character(GCAMreg)])

# --- Assign Super Region ---
region_lookup <- tibble::tibble(
  GCAM_Region_Name = unlist(region_groups),
  SuperRegion = rep(names(region_groups), times = lengths(region_groups))
)

filtered_data <- filtered_data %>%
  left_join(region_lookup, by = "GCAM_Region_Name") %>%
  filter(!is.na(SuperRegion))

# --- Assign Super Item Category ---
filtered_data <- filtered_data %>%
  rowwise() %>%
  mutate(
    SuperItem = {
      match <- NULL
      for (grp in names(item_groups)) {
        if (Item %in% item_groups[[grp]]) {
          match <- grp
          break
        }
      }
      if (is.null(match)) Item else match
    }
  ) %>%
  ungroup()

# --- Aggregate by SuperRegion and SuperItem ---
aggregated_data <- filtered_data %>%
  group_by(SuperRegion, GCAM_Region_Name, SuperItem, Year.Code) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  mutate(Value = Value / 1000)  # Convert to tonnes


# --- Plot Using facet_wrap() ---
for (super_region in unique(aggregated_data$SuperRegion)) {
  plot_data <- aggregated_data %>%
    filter(SuperRegion == super_region)

  if (nrow(plot_data) < 2) next

  p <- ggplot(plot_data, aes(x = Year.Code, y = Value, color = GCAM_Region_Name)) +
    geom_line(size = 1) +
    facet_wrap(~ SuperItem, scales = "free_y", ncol = 3) +
    labs(
      title = paste("Livestock Items in", super_region),
      x = "Year",
      y = "Manure N applied to soils (tonnes)",
      color = "Region"
    ) +
    scale_y_log10(labels = comma_format(), expand = c(0, 0)) +
    scale_color_manual(values = c("red3", "green3", "blue3", "yellow3", "purple3","orange3"))+
    #theme_minimal(base_size = 14)
    theme_bw() +
    theme(legend.position = "bottom",
          legend.direction = "horizontal")

  filename <- paste0(gsub("[^A-Za-z0-9]", "_", super_region), "_facet.png")
  ggsave(
    filename = file.path(output_dir, filename),
    plot = p,
    width = 10, height = 8, dpi = 300
  )

  cat("✅ Saved:", filename, "\n")
}

cat("🎉 All super region facet graphs saved to:", output_dir, "\n")
