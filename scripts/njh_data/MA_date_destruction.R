
library(tidyverse)
file <- "C:\\Users\\rodrica2\\OneDrive - The University of Colorado Denver\\Documents\\DFM\\projects\\hsq\\analyses_sas\\data\\njh_data_20260219.csv"
data <- read_csv(file, show_col_types = FALSE)

# Modify the MA data to remove the day from the date
data %<>%
  mutate(
    WebIntakeDate_mmyy = WebIntakeDate,
    PhoneIntakeDate_mmyy = PhoneIntakeDate) %>%
  mutate(across(WebIntakeDate_mmyy:PhoneIntakeDate_mmyy, ~ format(as.Date(.x, "%m/%d/%Y"), "%m/%Y"))) %>%
  mutate(across(WebIntakeDate:PhoneIntakeDate, ~ ifelse(`State Client` == "MA", NA, .x)))

write_csv(data, "C:\\Users\\rodrica2\\OneDrive - The University of Colorado Denver\\Documents\\DFM\\projects\\hsq\\analyses_sas\\data\\njh_data_20260219_deid.csv")