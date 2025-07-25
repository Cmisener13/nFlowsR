#box and whisker plot
# --- Load Required Libraries ---
library(ggplot2)
library(dplyr)
library(readr)

# --- Load Data ---
data <- read_csv("C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/Original_data_GCAMreg.csv",
                 locale = locale(encoding = "latin1"))

# --- Define Grouping ---
data <- data %>%
  mutate(Item_Group = case_when(
    Item %in% c("Swine, breeding", "Swine, market") ~ "Swine",
    Item %in% c("Chickens, broilers", "Chickens, layers", "Turkeys", "Ducks") ~ "Poultry",
    Item %in% c("Sheep", "Goats") ~ "Sheep and Goats",
    Item %in% c("Cattle, dairy") ~ "Dairy",
    Item %in% c("Cattle, non-dairy") ~ "Beef",

    TRUE ~ NA_character_
  ))

# --- Filter and Average ---
averaged_data <- data %>%
  filter(!is.na(Value), !is.na(Item_Group), !is.na(GCAMreg)) %>%
  group_by(GCAMreg, Item_Group) %>%
  summarise(Average_Value = mean(Value, na.rm = TRUE), .groups = "drop")

# --- Plot ---
plot <- ggplot(averaged_data, aes(x = Item_Group, y = Average_Value)) +
  geom_boxplot(outlier.shape = NA, coef = 0, fill = "#009E73", color = "black") +
  scale_y_continuous(
    limits = c(0, 15000000),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Average Livestock Values by Region and Item Group (No Outliers)",
    x = "Item Group",
    y = "Average Value"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- Save as JPEG ---
jpeg("C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/Original_data_GCAMregBoxNWhisker.png",
     width = 1000, height = 1000, res = 150)
print(plot)
dev.off()

cat("✅ JPEG saved to: C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/OOriginaldataBoxNWhisker.jpeg")
