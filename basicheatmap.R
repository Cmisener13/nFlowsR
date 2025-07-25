#heatmap
library(tidyverse)
library(sf)
library(ggplot2)
library(gganimate)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)

# Load data
data <- read_csv("C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/Original_data_with_population.csv")

# Define livestock categories and years of interest
livestock_map <- list(
  "Dairy" = "Cattle, dairy",
  "Beef" = "Cattle, non-dairy",
  "Swine" = c("Swine, breeding", "Swine, market"),
  "Poultry" = c("Chickens, broilers", "Chickens, layers", "Ducks", "Turkeys"),
  "Sheep and Goats" = c("Sheep", "Goats")
)
target_years <- c(1971, 1981, 1991, 2001, 2011, 2021)

# Reverse mapping for recode
item_to_category <- unlist(lapply(names(livestock_map), function(cat) setNames(rep(cat, length(livestock_map[[cat]])), livestock_map[[cat]])))

# Filter and preprocess
data_filtered <- data %>%
  filter(Item %in% names(item_to_category), Year.Code %in% target_years) %>%
  mutate(
    Year = Year.Code,
    Livestock = recode(Item, !!!item_to_category),
    population = as.numeric(population),
    Value = as.numeric(`Value...4`),
    PerCapita = Value / population
  ) %>%
  drop_na(PerCapita)

# Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge with world map
map_data <- world %>%
  left_join(data_filtered, by = c("name" = "Area"))

# Function to plot individual maps
plot_heatmap <- function(df, livestock_type, year) {
  df_year <- df %>% filter(Livestock == livestock_type, Year == year)
  ggplot(df_year) +
    geom_sf(aes(fill = PerCapita), color = "grey50") +
    scale_fill_viridis(option = "inferno", na.value = "white", name = "Per Capita") +
    labs(title = paste(livestock_type, "Per Capita -", year)) +
    theme_minimal()
}

# Save individual heatmaps
for (livestock_type in unique(data_filtered$Livestock)) {
  for (year in target_years) {
    p <- plot_heatmap(map_data, livestock_type, year)
    ggsave(filename = paste0("C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/PerCapitaMapTotal_", livestock_type, "_", year, ".png"), plot = p, width = 10, height = 6)
  }
}

# Faceted map for each livestock type
for (livestock_type in unique(data_filtered$Livestock)) {
  p <- ggplot(map_data %>% filter(Livestock == livestock_type)) +
    geom_sf(aes(fill = PerCapita), color = "grey70") +
    scale_fill_viridis(option = "plasma", na.value = "white") +
    facet_wrap(~Year) +
    labs(title = paste("Per Capita for", livestock_type), fill = "Per Capita") +
    theme_minimal()
  ggsave(filename = paste0("C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/PerCapitaMapFacet_", livestock_type, ".png"), plot = p, width = 14, height = 8)
}

# Animated map
for (livestock_type in unique(data_filtered$Livestock)) {
  p_anim <- ggplot(map_data %>% filter(Livestock == livestock_type)) +
    geom_sf(aes(fill = PerCapita), color = "grey40") +
    scale_fill_viridis(option = "magma", trans = "log", na.value = "white", direction =1) +
    labs(title = paste("Per Capita -", livestock_type, "in {closest_state}"), fill = "Per Capita") +
    theme_minimal()

    transition_states(Year, transition_length = 2, state_length = 1) +
    ease_aes('cubic-in-out')

  gif <- animate(anim_plot, width = 800, height = 500, renderer = gifski_renderer())
  anim_save(filename = paste0("C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/PerCapitaMapGif_", livestock_type, ".gif"), animation = gif)}
