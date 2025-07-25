#each graph saved as png
# --- Load Required Libraries ---
library(ggplot2)
library(dplyr)
library(readr)
library(scales)

# --- GCAM Region Name Mapping ---
gcam_names <- c(
  "1" = "USA", "2" = "Africa_Eastern", "3" = "Africa_Northern", "4" = "Africa_Southern",
  "5" = "Africa_Western", "6" = "Australia_NZ", "7" = "Brazil",
  "8" = "Canada", "9" = "Central America and Caribbean", "10" = "Central Asia",
  "11" = "China", "12" = "EU-12", "13" = "EU-15",
  "14" = "Ukraine", "15" = "Europe_Non_EU", "16" = "European Free Trade Association", "17" = "India",
  "18" = "Indonesia", "19" = "Japan", "20" = "Mexico",
  "21" = "Middle East", "22" = "Pakistan", "23" = "Russia",
  "24" = "South Africa", "25" = "South America_Northern", "26" = "South America_Southern",
  "27" = "South Asia", "28" = "South Korea", "29" = "Southeast Asia",
  "30" = "Taiwan", "31" = "Argentina", "32" = "Colombia"
)

# --- Set File Paths ---
data_file <- "C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/Original_data_GCAMreg.csv"
output_dir <- "C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/goodRegGraphsPNG.png"
dir.create(output_dir, showWarnings = FALSE)

# --- Load Data ---
data <- read_csv(data_file, locale = locale(encoding = "latin1"))

# --- Filter and Label ---
filtered_data <- data %>%
  filter(
    !is.na(GCAMreg),
    GCAMreg %in% 1:32,
    Year.Code >= 1961,
    Year.Code <= 2022
  ) %>%
  mutate(GCAM_region = gcam_names[as.character(GCAMreg)])

# --- Identify Top 10 Items ---
top_items <- filtered_data %>%
  count(Item, sort = TRUE) %>%
  top_n(10, n) %>%
  pull(Item)

# --- Aggregate Data to Avoid Duplicates ---
aggregated_data <- filtered_data %>%
  filter(Item %in% top_items) %>%
  group_by(GCAM_region, Item, Year.Code) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")

# --- Loop and Save All 320 Graphs as PNG ---
for (region in unique(aggregated_data$GCAM_region)) {
  for (item in top_items) {

    plot_data <- aggregated_data %>%
      filter(GCAM_region == region, Item == item)

    if (nrow(plot_data) < 2) next  # Skip empty/sparse plots

    p <- ggplot(plot_data, aes(x = Year.Code, y = Value)) +
      geom_line(color = "#0072B2", size = 1) +
      labs(
        title = paste(item, "in", region),
        x = "Year",
        y = "Value"
      ) +
      scale_y_log10(labels = comma_format(), expand = c(0, 0)) +
      theme_minimal(base_size = 14)

    # Save PNG
    filename <- paste0(gsub("[^A-Za-z0-9]", "_", region), "_", gsub("[^A-Za-z0-9]", "_", item), ".png")
    ggsave(
      filename = file.path(output_dir, filename),
      plot = p,
      width = 8, height = 5, dpi = 300
    )

    cat("✅ Saved:", filename, "\n")
  }
}

cat("🎉 All PNGs saved to:", output_dir, "\n")
