 library(tidyverse)
 library(dplyr)
 library(tidyr)

 HISTORICAL_YEARS <- 1961:2022

 # Define the interpolation function
approx_fun <- function(x, y, rule = 2) {
   if (all(is.na(y) | y == 0)) return(rep(NA, length(y)))
   approx(x[!is.na(y) & y != 0],
          y[!is.na(y) & y != 0],
          xout = x,
          rule = rule,
          ties = mean)$y
 }

 # Process the data
expanded_data <- read.csv("C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/expanded_fao_data.csv") %>%
  complete(nesting(Area), Year.Code = HISTORICAL_YEARS) %>%
  arrange(Area, Year.Code) %>%
  group_by(Area) %>%
  mutate(Value = approx_fun(Year.Code, Value)) %>%
  ungroup()

 # Write to CSV
 write.csv(expanded_data, "C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/New_expanded_fao_data.csv", row.names = FALSE)

 # Read back in
 Iter.6 <- read_csv("C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/New_expanded_fao_data.csv")

