#saved to png correct regions
# --- Load Required Libraries ---
library(ggplot2)
library(dplyr)
library(readr)
library(scales)

# --- Set File Paths ---
data_file <- "C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/Original_data_GCAMreg.csv"
output_dir <- "C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/goodRegGraphsPNG.png"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# --- Load Data ---
data <- read_csv(data_file, locale = locale(encoding = "latin1"))

# --- Check column names ---
print("Column names in data:")
print(names(data))

# --- Use actual region column name ---
# Replace "region" with the actual column name if different (e.g., "Region", "GCAM_region")
region_col <- "region"  # Change this if needed

# --- Check that region column exists ---
if (!region_col %in% names(data)) {
  stop(paste("Column", region_col, "not found in data. Please check the column names."))
}

# --- Filter and Label ---
filtered_data <- data %>%
  filter(
    !is.na(.data[[region_col]]),
    Year.Code >= 1961,
    Year.Code <= 2022
  )

# --- Identify Top 10 Items ---
top_items <- filtered_data %>%
  count(Item, sort = TRUE) %>%
  top_n(10, n) %>%
  pull(Item)

print("Top 10 items:")
print(top_items)

# --- Aggregate Data ---
aggregated_data <- filtered_data %>%
  filter(Item %in% top_items) %>%
  group_by(region = .data[[region_col]], Item, Year.Code) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")

# --- Save Graphs ---
count <- 0
for (region_name in unique(aggregated_data$region)) {
  for (item in top_items) {
    plot_data <- aggregated_data %>%
      filter(region == region_name, Item == item)

    if (nrow(plot_data) < 2) next

    p <- ggplot(plot_data, aes(x = Year.Code, y = Value)) +
      geom_line(color = "#0072B2", size = 1) +
      labs(
        title = paste(item, "in", region_name),
        x = "Year",
        y = "Value"
      ) +
      scale_y_log10(labels = comma_format(), expand = c(0, 0)) +
      theme_minimal(base_size = 14)

    filename <- paste0(gsub("[^A-Za-z0-9]", "_", region_name), "_", gsub("[^A-Za-z0-9]", "_", item), ".png")
    filepath <- file.path(output_dir, filename)

    ggsave(
      filename = filepath,
      plot = p,
      width = 8, height = 5, dpi = 300
    )

    cat("✅ Saved:", filepath, "\n")
    count <- count + 1
  }
}

cat("🎉 Total graphs saved:", count, "\n")
cat("📁 Output folder:", output_dir, "\n")
