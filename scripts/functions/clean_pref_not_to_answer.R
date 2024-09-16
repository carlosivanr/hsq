# Clean Prefer not to answer %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Description:
# Some values in the data have responses that include "Prefer not to answer".
# This function will go through columns and convert values these responses to NA

# Usage:
# data <- clean_ptna(data, vector_of_cols)
# where data is the input data frame and vector of cols is a character vector
# of the column names to modify
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# This function will replace any values that contain "Prefer not answer" with NA
clean_no_answer <- function(data, vector_of_cols){
  data %<>%
    mutate(across(all_of(vector_of_cols), ~ if_else(.x == "Prefer to not answer", NA, .x)),
           across(all_of(vector_of_cols), ~ if_else(.x == "Prefer not to answer", NA, .x)),
           across(all_of(vector_of_cols), ~ droplevels(.x)))
}
