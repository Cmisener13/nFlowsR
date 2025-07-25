#USA only
# --- Load Required Libraries ---
library(ggplot2)
library(dplyr)
library(readr)
library(scales)

# --- Set File Paths ---
data_file <- "C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/Original_data_GCAMreg.csv"
output_dir <- "C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/USAtonneGraphs"
dir.create(output_dir, showWarnings = FALSE)

# --- Load Data ---
data <- read_csv(data_file, locale = locale(encoding = "latin1"))

# --- GCAM Region Code to Name Mapping ---
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
  "Beef" = c("Cattle, non-dairy"),
  "Dairy" = c("Cattle, dairy")
)

# --- Filter and Clean Data (USA only) ---
filtered_data <- data %>%
  filter(GCAMreg == 1, Year.Code >= 1961, Year.Code <= 2022) %>%
  mutate(
    GCAM_Region_Name = gcam_names[as.character(GCAMreg)],
    Value = as.numeric(Value) / 1e3  # Convert from kg to tonnes
  )

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

# --- Aggregate by SuperItem and Year (USA only) ---
usa_totals <- filtered_data %>%
  group_by(SuperItem, Year.Code) %>%
  summarise(TotalValue = sum(Value, na.rm = TRUE), .groups = "drop")

# --- Single Facet-Wrapped Plot for USA ---
p <- ggplot(usa_totals, aes(x = Year.Code, y = TotalValue)) +
  geom_line(color = "firebrick", size = 1.2) +
  facet_wrap(~ SuperItem, scales = "free_y", ncol = 3) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "USA Manure N Applied to Soils Over Time",
    x = "Year",
    y = "Manure N applied to soils(tonnes)"
  ) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")

# --- Save the Image ---
filename <- file.path(output_dir, "USA_Totals_tonnes.png")
ggsave(
  filename = filename,
  plot = p,
  width = 12, height = 8, dpi = 300
)

cat("✅ Saved:", filename, "\n")
