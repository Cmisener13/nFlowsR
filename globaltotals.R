#global totals
# --- Load Required Libraries ---
library(ggplot2)
library(dplyr)
library(readr)
library(scales)

# --- Set File Paths ---
data_file <- "C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/Original_data_GCAMreg.csv"
output_dir <- "C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/WorldGraphTonnes"
dir.create(output_dir, showWarnings = FALSE)

# --- Load Data ---
data <- read_csv(data_file, locale = locale(encoding = "latin1"))

# --- Livestock Groupings ---
item_groups <- list(
  "Swine" = c("Swine, breeding", "Swine, market"),
  "Poultry" = c("Chickens, broilers", "Chickens, layers", "Ducks", "Turkeys"),
  "Sheep & Goats" = c("Sheep", "Goats"),
  "Beef" = c("Cattle, non-dairy"),
  "Dairy" = c("Cattle, dairy")
)

# --- Filter and Clean Data ---
filtered_data <- data %>%
  filter(!is.na(GCAMreg), GCAMreg %in% 1:32, Year.Code >= 1961, Year.Code <= 2022) %>%
  mutate(Value = as.numeric(Value),     # Ensure Value is numeric
         Value = Value / 1e9)           # Convert to gigagrams (Gg) or megatonnes

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

# --- Aggregate Globally by SuperItem and Year ---
global_totals <- filtered_data %>%
  group_by(SuperItem, Year.Code) %>%
  summarise(GlobalTotal = sum(Value, na.rm = TRUE), .groups = "drop")

# --- Single Facet-Wrapped Plot ---
p <- ggplot(global_totals, aes(x = Year.Code, y = GlobalTotal)) +
  geom_line(color = "steelblue", size = 1.2) +
  facet_wrap(~ SuperItem, scales = "free_y", ncol = 3) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Global Manure N Applied to Soils Over Time",
    x = "Year",
    y = "Manure N applied to soils (Megatonnes)"
  ) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")

# --- Save the Single Image ---
filename <- file.path(output_dir, "GlobalTotalsMegatonnes.png")
ggsave(
  filename = filename,
  plot = p,
  width = 12, height = 8, dpi = 300
)

cat("✅ Saved:", filename, "\n")
