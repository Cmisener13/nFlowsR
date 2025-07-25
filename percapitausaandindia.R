#per capita USA and India
# --- Load Required Libraries ---
library(ggplot2)
library(dplyr)
library(readr)
library(scales)

# --- Set File Paths ---
data_file <- "C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/Original_data_with_population.csv"
output_dir <- "C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/PerCapitaUSAIndia"
dir.create(output_dir, showWarnings = FALSE)

# --- Load Data ---
data <- read_csv(data_file, locale = locale(encoding = "latin1"))

# --- Livestock Groupings ---
item_groups <- list(
  "Swine" = c("Swine, breeding", "Swine, market"),
  "Poultry" = c("Chickens, broilers", "Chickens, layers", "Ducks", "Turkeys"),
  "Sheep & Goats" = c("Sheep", "Goats"),
  "Dairy" = c("Cattle, dairy"),
  "Beef" = c("Cattle, non-dairy")
)

# --- Filter and Normalize Data (Only USA and India) ---
normalized_data <- data %>%
  filter(Area %in% c("United States of America", "India"),
         Year.Code >= 1971, Year.Code <= 2021,
         !is.na(Value...4), !is.na(population)) %>%
  mutate(Value_per_Capita = Value...4 / population)

# --- Assign Super Item Category ---
normalized_data <- normalized_data %>%
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

# --- Aggregate Normalized Values (per Area and SuperItem) ---
aggregated_data <- normalized_data %>%
  group_by(Area, SuperItem, Year.Code) %>%
  summarise(Value = sum(Value_per_Capita, na.rm = TRUE), .groups = "drop")

# --- Generate One Graph per Area with All Super Items ---
for (area in unique(aggregated_data$Area)) {

  plot_data <- aggregated_data %>%
    filter(Area == area)

  p <- ggplot(plot_data, aes(x = Year.Code, y = Value, color = SuperItem)) +
    geom_line(size = 1) +
    labs(
      title = paste("Per Capita Livestock Categories in", area),
      x = "Year",
      y = "Manure N per Capita (kg)",
      color = "Category"
    ) +
    scale_y_log10(labels = comma_format(), expand = c(0, 0)) +
    theme_minimal(base_size = 14)

  filename <- paste0(gsub("[^A-Za-z0-9]", "_", area), "_All_Livestock.png")
  ggsave(
    filename = file.path(output_dir, filename),
    plot = p,
    width = 10, height = 6, dpi = 300
  )

  cat("✅ Saved:", filename, "\n")
}

cat("🎉 Combined graphs for USA and India saved to:", output_dir, "\n")
