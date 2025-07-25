#extrapolation
library(tidyverse)
library(dplyr)
library(tidyr)




approx_linear_extrapolate <- function(x, y, xout) {
  valid <- !is.na(y) & y != 0
  if (sum(valid) < 2) {
    return(rep(NA, length(xout)))  # not enough data to do anything
  }

  x_valid <- x[valid]
  y_valid <- y[valid]

  # Perform interpolation for xout within range
  interp <- approx(x = x_valid, y = y_valid, xout = xout, rule = 1, ties = mean)$y

  # Handle extrapolation manually
  min_x <- min(x_valid)
  max_x <- max(x_valid)

  # Linear extrapolation before min_x
  before_idx <- which(xout < min_x)
  if (length(before_idx) > 0) {
    slope_start <- (y_valid[2] - y_valid[1]) / (x_valid[2] - x_valid[1])
    intercept_start <- y_valid[1] - slope_start * x_valid[1]
    interp[before_idx] <- slope_start * xout[before_idx] + intercept_start
  }

  # Linear extrapolation after max_x
  after_idx <- which(xout > max_x)
  if (length(after_idx) > 0) {
    n <- length(y_valid)
    slope_end <- (y_valid[n] - y_valid[n - 1]) / (x_valid[n] - x_valid[n - 1])
    intercept_end <- y_valid[n] - slope_end * x_valid[n]
    interp[after_idx] <- slope_end * xout[after_idx] + intercept_end
  }

  return(interp)
}



interp_fun <- function(x, y) {
  approx_linear_extrapolate(x, y, x)
}




expanded_data <- read.csv("C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/expanded_fao_data.csv") %>%
  complete(nesting(Area, Item), Year.Code = 1961:2022) %>%
  arrange(Area, Item, Year.Code) %>%
  group_by(Area, Item) %>%
  mutate(Value = interp_fun(Year.Code, Value)) %>%
  ungroup()

# Write to CSV
write.csv(expanded_data, "C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/1st_expanded_fao_data.csv", row.names = FALSE)

# Read back in
Iter.7 <- read_csv("C:/Users/mise017/OneDrive - PNNL/Documents/nFlowsR/1st_expanded_fao_data.csv")

