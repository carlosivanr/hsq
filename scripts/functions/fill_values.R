# Fill values rows %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

fill_values <- function(data, var){
  # Get the names of the columns
  col_names <- colnames(data)
  
  # Get the values to be merged in
  values_to_merge <- 
    data %>%
    select(record_id, !!rlang::sym(var)) %>%
    group_by(record_id) %>%
    slice_head()
  
  # Merge in the values
  data %<>%
    select(-!!rlang::sym(var)) %>%
    left_join(., values_to_merge, by = "record_id")
  
  # Restore the order of the col names
  data %<>%
    select(all_of(col_names))
  
  # Return the data frame
  return(data)
  
  }


