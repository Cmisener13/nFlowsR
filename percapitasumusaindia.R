#per capita sum India, USA
# --- Load Required Libraries ---
library(ggplot2)
library(dplyr)
library(readr)
library(scales)

# --- Set File Paths ---
data_file <- "C:/Users/mise017/OneDrive -PNNL/Documents/nFlowsR/Original_data_with_population.csv"
output_dir <- "C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/PerCapita_AvgCountryLine"
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

# --- Filter and Normalize Data (Only USA and China) ---
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

# --- Sum total per capita values per country and year ---
country_sum <- normalized_data %>%
  group_by(Area, Year.Code) %>%
  summarise(SumPerCapita = sum(Value_per_Capita, na.rm = TRUE), .groups = "drop")

# --- Plot both country lines on a single graph ---
p <- ggplot(country_sum, aes(x = Year.Code, y = SumPerCapita, color = Area)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("United States of America" = "red3", "India" = "green2")) +
  scale_y_log10(labels = comma_format(), expand = c(0, 0)) +
  labs(
    title = "Summed Per Capita Livestock Totals (1971-2021)",
    x = "Year",
    y = "Summed Manure N per Capita (kg)",
    color = "Country"
  ) +
  theme_minimal(base_size = 14)

# --- Save the plot ---
ggsave(
  filename = file.path(output_dir, "SumPerCapita_USA_vs_China.png"),
  plot = p,
  width = 10, height = 6, dpi = 300
)

cat("\u2705 Saved: SumPerCapita_USA_vs_India.png\n")
