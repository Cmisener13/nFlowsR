#2021-1971 heatmap
# Libraries
library(tidyverse)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)

# Load data
data <- read_csv("C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/Original_data_with_population.csv")

# Livestock mapping
livestock_map <- list(
  "Dairy" = "Cattle, dairy",
  "Beef" = "Cattle, non-dairy",
  "Swine" = c("Swine, breeding", "Swine, market"),
  "Poultry" = c("Chickens, broilers", "Chickens, layers", "Ducks", "Turkeys"),
  "Sheep and Goats" = c("Sheep", "Goats")
)

# Reverse mapping
item_to_category <- unlist(lapply(names(livestock_map), function(cat) setNames(rep(cat, length(livestock_map[[cat]])), livestock_map[[cat]])))

# Filter and compute per capita
data_filtered <- data %>%
  filter(Item %in% names(item_to_category), Year.Code %in% c(1971, 2021)) %>%
  mutate(
    Year = Year.Code,
    population = as.numeric(population),
    Value = as.numeric(`Value...4`),
    PerCapita = Value / population
  ) %>%
  drop_na(PerCapita)

# Summarize total per capita by area and year (sum over all livestock types)
data_summary <- data_filtered %>%
  group_by(Area, Year) %>%
  summarize(TotalPerCapita = sum(PerCapita), .groups = 'drop')

# Spread and compute difference
percapita_diff <- data_summary %>%
  pivot_wider(names_from = Year, values_from = TotalPerCapita, names_prefix = "Year_") %>%
  mutate(Diff = Year_2021 - Year_1971)

# Merge with world map
world <- ne_countries(scale = "medium", returnclass = "sf")
map_data <- world %>%
  left_join(percapita_diff, by = c("name" = "Area"))

# Plot difference map
p <- ggplot(map_data) +
  geom_sf(aes(fill = sign(Diff) * log10(abs(Diff) + 1)), color = "grey50") +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0,
    name = "Log10 Diff\n(2021 - 1971)"
  ) +
  labs(title = "Change in Total Livestock Per Capita (2021 - 1971)") +
  theme_minimal()

# Ensure plot is printed
print(p)

# Save the plot as PNG with explicit device
output_path <- "C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/PerCapitaChange_Total_LogMap.png"
ggsave(
  filename = output_path,
  plot = p,
  device = "png",
  width = 12,
  height = 7,
  dpi = 300
)

message("Plot saved to: ", output_path)
