#assigning GCAM regions
library("tidyverse")
library(dplyr)
library(tidyr)

# Load the data files
fao_data <- read.csv("C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/2nd_fao_data.csv")
gcam_ids <- read.csv("C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/GCAMregID.csv")

# Print column names to debug
print(colnames(gcam_ids))

# Trim whitespace from column names just in case
colnames(gcam_ids) <- trimws(colnames(gcam_ids))

# Check if 'Area' and 'GCAM_region_ID' exist after trimming
if (!("Area" %in% colnames(gcam_ids)) || !("GCAM_region_ID" %in% colnames(gcam_ids))) {
  stop("Required columns 'Area' or 'GCAM_region_ID' not found in GCAMregID.csv")
}

# Rename 'Area' to 'area' for merging
names(gcam_ids)[names(gcam_ids) == "Area"] <- "area"
# Ensure both data frames have 'area' column
colnames(fao_data) <- trimws(colnames(fao_data))
colnames(gcam_ids) <- trimws(colnames(gcam_ids))
names(gcam_ids)[names(gcam_ids) == "Area"] <- "area"
names(fao_data)[names(fao_data) == "Area"] <- "area"  # change if it's differently named

# Merge and add GCAMreg column
fao_data <- left_join(fao_data, gcam_ids[, c("area", "GCAM_region_ID")], by = "area")
fao_data$GCAMreg <- fao_data$GCAM_region_ID
fao_data$GCAM_region_ID <- NULL

# Save the updated dataset
write.csv(fao_data, "C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/1stGCAMreg.csv", row.names = FALSE)

print("GCAMreg column updated and file saved as '1stGCAMreg.csv'")

withGCAMreg <- read_csv("1stGCAMreg.csv")

