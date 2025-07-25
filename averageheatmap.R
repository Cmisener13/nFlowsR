#average heatmap
# --- Libraries ---
library(tidyverse)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)

# --- Load Data ---
data <- read_csv("C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/Original_data_with_population.csv")

# --- Define livestock categories ---
livestock_map <- list(
  "Dairy" = "Cattle, dairy",
  "Beef" = "Cattle, non-dairy",
  "Swine" = c("Swine, breeding", "Swine, market"),
  "Poultry" = c("Chickens, broilers", "Chickens, layers", "Ducks", "Turkeys"),
  "Sheep and Goats" = c("Sheep", "Goats")
)

# --- Reverse mapping ---
item_to_category <- unlist(lapply(names(livestock_map), function(cat) setNames(rep(cat, length(livestock_map[[cat]])), livestock_map[[cat]])))

# --- Filter and preprocess data (1971-2021) ---
data_filtered <- data %>%
  filter(Item %in% names(item_to_category), Year.Code >= 1971, Year.Code <= 2021) %>%
  mutate(
    Livestock = recode(Item, !!!item_to_category),
    population = as.numeric(population),
    Value = as.numeric(`Value...4`),
    PerCapita = Value / population
  ) %>%
  drop_na(PerCapita)

# --- Average per country/livestock/year ---
data_avg <- data_filtered %>%
  group_by(Area, Livestock, Year = Year.Code) %>%
  summarise(AvgPerCapita = mean(PerCapita, na.rm = TRUE), .groups = 'drop')

# --- Merge with world map ---
world <- ne_countries(scale = "medium", returnclass = "sf")
map_data <- world %>%
  left_join(data_avg, by = c("name" = "Area"))

# --- Create faceted heatmap with five livestock categories ---
p <- ggplot(map_data) +
  geom_sf(aes(fill = AvgPerCapita), color = "grey60") +
  scale_fill_viridis(option = "plasma", trans = "log", na.value = "white", name = "Per Capita (log scale)") +
  facet_wrap(~Livestock) +
  labs(title = "Average Per Capita Livestock (1971-2021)") +
  theme_minimal()

# --- Save plot ---
ggsave(
  filename = "C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/PerCapitaMap_Facet_All.png",
  plot = p,
  width = 16,
  height = 10
)
