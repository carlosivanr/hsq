# Clean blanks %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Description:
# Some values in the data have responses that include "Prefer not to answer".
# This function will go through columns and convert values these responses to NA

# Dependencies:
# magrittr
# dplyr

# Usage:
# data <- clean_blanks(data, vector_of_cols)
# where data is the input data frame and vector of cols is a character vector
# of the column names to modify
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# This function will replace any values that contain "Prefer not answer" with NA
clean_blanks <- function(data, vector_of_cols){
  data %<>%
    mutate(across(vector_of_cols, ~ na_if(.x, "")))
}