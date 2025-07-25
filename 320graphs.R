#320 graphs, original data, GCAM region
# --- Load Required Libraries ---
library(ggplot2)
library(dplyr)
library(readr)
library(plotly)
library(htmlwidgets)
library(scales)

# --- GCAM Region Name Mapping ---
gcam_names <- c(
  "1" = "USA", "2" = "Canada", "3" = "Western Europe", "4" = "Japan",
  "5" = "Australia_NZ", "6" = "Former Soviet Union", "7" = "Eastern Europe",
  "8" = "Middle East", "9" = "Africa_Eastern", "10" = "Africa_Northern",
  "11" = "Africa_Southern", "12" = "Africa_Western", "13" = "South Africa",
  "14" = "India", "15" = "China", "16" = "Korea", "17" = "Indonesia",
  "18" = "Other East Asia", "19" = "Thailand", "20" = "Vietnam",
  "21" = "Rest of Southeast Asia", "22" = "Brazil", "23" = "Mexico",
  "24" = "Argentina", "25" = "Rest of Central America", "26" = "Colombia",
  "27" = "Rest of South America", "28" = "EU-12", "29" = "Turkey",
  "30" = "Ukraine", "31" = "Philippines", "32" = "Pakistan"
)

# --- File Paths and Parameters ---
data_file <- "C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/New_expanded_GCAM_fao_data.csv"
output_dir <- "C:/Users/mise017/Desktop/GCAM_graphs_fixed_duplicates"
dir.create(output_dir, showWarnings = FALSE)

# Number of graphs to open in browser
open_first_n <- 5

# --- Load and Prepare Data ---
data <- read_csv(data_file, locale = locale(encoding = "latin1"))

# --- Inspect names(data) if needed ---
# print(names(data))

filtered_data <- data %>%
  filter(
    !is.na(GCAMreg),
    GCAMreg %in% 1:32,
    Year.Code >= 1961,
    Year.Code <= 2022
  ) %>%
  mutate(GCAM_region = gcam_names[as.character(GCAMreg)])

# --- Top 10 Items Based on Frequency ---
top_items <- filtered_data %>%
  count(Item, sort = TRUE) %>%
  top_n(10, n) %>%
  pull(Item)

top_data <- filtered_data %>%
  filter(Item %in% top_items)

# --- Aggregate to Avoid Duplicates ---
aggregated_data <- top_data %>%
  group_by(GCAM_region, Item, Year.Code) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")

# --- Generate Graphs for Each Region × Item ---
opened_count <- 0

for (region in unique(aggregated_data$GCAM_region)) {
  for (item in top_items) {

    plot_data <- aggregated_data %>%
      filter(GCAM_region == region, Item == item)

    if (nrow(plot_data) < 2) next  # Skip sparse

    # --- Build Plot ---
    p <- ggplot(plot_data, aes(
      x = Year.Code,
      y = Value,
      text = paste("Year:", Year.Code, "<br>Value:", signif(Value, 4))
    )) +
      geom_line(color = "#1f77b4", linewidth = 1, alpha = 0.9) +
      labs(
        title = paste(item, "in", region),
        x = "Year",
        y = "Value"
      ) +
      scale_y_log10(labels = comma_format(), expand = c(0, 0)) +
      theme_minimal()

    # --- Make Interactive ---
    interactive_plot <- ggplotly(p, tooltip = "text") %>%
      layout(hovermode = "x unified")

    # --- Save File ---
    file_name <- paste0(gsub("[^A-Za-z0-9]", "_", region), "_", gsub("[^A-Za-z0-9]", "_", item), ".html")
    full_path <- file.path(output_dir, file_name)
    saveWidget(interactive_plot, full_path, selfcontained = TRUE)

    if (opened_count < open_first_n) {
      browseURL(full_path)
      opened_count <- opened_count + 1
    }

    cat("✅ Saved:", full_path, "\n")
  }
}

cat("📁 All graphs saved to:", output_dir, "\n")
cat("🌐", opened_count, "opened in browser.\n")
cat("✏️ Adjust `open_first_n` at top to open more.\n")
