# REDCap Survey Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Requires:
# 1. a data frame processed with Redcap's R script
# 2. a data frame that captures all of the labels
      # After applying RedCaps data processing script this command captures 
      # all labels from non-factored columns
      # labels <- data %>%
      #   select(!contains(".factor"))  %>%
      #   map_df(., ~attr(.x, "label"))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pacman::p_load(
  tidyverse,
  here,
  magrittr,
  gtsummary,
  install = FALSE)

######################## TABLES ################################################



#1. Radio ----------------------------------------------------------------------
# Inputs
# 1. needs a data frame with the data
# 1. needs a data frame with the corresponding labels

# make table function development, for factored questions
# Requires the .factor columns to be piped into as the principal input.
# Example:
# data %>%
#   select(changesinweight_q2.factor, pm_role.factor) %>%
#   make_tbl_fct(., labels, group = "pm_role.factor")


make_tbl_fct <- function(data, labels, group = NULL){

  # If there is not a grouping variable
  if(is.null(group) == TRUE){
    
    # Get the names of the input variables
    vars <- names(data)
    
    # Check if all the vars end in .factor
    if(sum(grepl(".factor", vars)) != length(vars)){
      stop("Not all input variables are .factor! All input variables need to be .factor.")
    }
  
    # Set the labels of the variables
    labs <- as.character(labels %>%
                             select(all_of(
                               sub(".factor.*", "", vars))))
    
    # Make a gt_summary table                          
    tbl <- data %>%
      rename_at(all_of(vars), ~ labs,
                missing = "ifany") %>%
      tbl_summary()
    
  } else {
  # If there is a group
  # Then vars has to be all of the columns variables without the grouping column
    vars <- names(data)
    
    # Index all variables except for the group
    vars <- vars[!vars %in% group]
    
    # Check if all the vars end in .factor
    if(sum(grepl(".factor", vars)) != length(vars)){
      stop("Not all input variables are .factor! All input variables need to be .factor.")
    }
    
    # Set the labels of the variables
    labs <- as.character(labels %>%
                           select(all_of(
                             sub(".factor.*", "", vars))))
    
    # Make a gt_summary table with the "by" statement
    tbl <- data %>%
      rename_at(all_of(vars), ~ labs) %>%
      tbl_summary(by = group, 
                  missing = "ifany")
    
    }
  
  return(tbl)
}



#2. Checkbox -------------------------------------------------------------------
make_check_all_tbl <- function(data, vars, by_var = NULL, grouping_label){
  # This function was created to be able to parameterize making a checkbox
  # table while modifying the grouping_label
  
  # Create an index where the column sums are used to sort the variables -------
  index <- data %>% 
    select(all_of(vars)) %>%
    colSums() %>%
    order(decreasing = TRUE)
  
  # Create a new vector of column names ordered by overall frequency -----------
  ordered_vars <- vars[index]
  
  # Create a basic gtsummary table ---------------------------------------------
  tbl <- data %>%
    select(all_of(c(ordered_vars, by_var))) %>%
    tbl_summary(by = all_of(by_var))
  
  # Create a named set of arguments --------------------------------------------
  # unpacked add_variable_grouping() from bstfun bc couldn't get the
  # name of the vector to be named dynamically
  dots <- rlang::dots_list("grouping_label" = ordered_vars)
  #rlang::is_named(dots)
  
  # Convert the named set of arguments -----------------------------------------
  df_insert <- 
    dots %>%
    imap(~which(tbl$table_body$variable %in% .x[1])[1]) %>%
    unlist() %>%
    tibble::enframe("include_group", "insert_above_row_number")
  
  # Insert the grouping variable dynamically -----------------------------------
  df_insert[1] <- grouping_label
  
  for (i in rev(seq_len(nrow(df_insert)))) {
    tbl <-
      tbl %>%
      gtsummary::modify_table_body(
        ~ tibble::add_row(.x,
                          variable = df_insert$include_group[i],
                          label = df_insert$include_group[i],
                          row_type = "label",
                          .before = df_insert$insert_above_row_number[i])
      )
  }
  
  # update the indentation for the grouped variables ---------------------------
  tbl <-
    tbl %>%
    gtsummary::modify_table_styling(
      columns = "label",
      rows = .data$variable %in% unname(unlist(dots)) & .data$row_type %in% "label",
      text_format = "indent"
    ) %>%
    gtsummary::modify_table_styling(
      columns = "label",
      rows = .data$variable %in% unname(unlist(dots)) & !.data$row_type %in% "label",
      undo_text_format =  TRUE,
      text_format = "indent"
    ) %>%
    gtsummary::modify_table_styling(
      columns = "label",
      rows = .data$variable %in% unname(unlist(dots)) & !.data$row_type %in% "label",
      text_format = "indent2"
    )
  
  # return final tbl -----------------------------------------------------------
  return(tbl)
}

##################### FENCE ####################################################

#3. Radio Matrix, good for tbl_likert ------------------------------------------
# Inputs
# 1. needs a data frame with the data
# 1. needs a data frame with the corresponding labels
# Requires the .factor columns to be piped into as the principal input.
# Example:

# data %>%
#   select(provide_wls.factor:appreciation.factor) %>%
#   make_tbl_likert(., labels, title = "Questions Foo!!!")

make_tbl_likert <- function(data, labels, title = NULL){
  # Tbl_likert is not appropriate if you want to see just the number of missing
  # responses without including them into a percentage count
  
  # Get the names of the input variables
  vars <- names(data)
  
  # Set the labels of the variables
  labs <- as.character(labels %>%
                         select(all_of(
                           sub(".factor.*", "", vars))))
  
  
  if(is.null(title) == TRUE){
    tbl <- data %>%
      #mutate_at(all_of(vars), ~ fct_explicit_na(.x)) %>%
      rename_at(all_of(vars), ~ labs) %>%
      bstfun::tbl_likert() %>%
      add_n()
    
  } else {
    
    # Bold the question title
    title <- str_c("**", title, "**")
    
    tbl <- data %>%
      #mutate_at(all_of(vars), ~ fct_explicit_na(.x)) %>%
      rename_at(all_of(vars), ~ labs) %>%
      bstfun::tbl_likert() %>%
      modify_header(label = title) %>%
      add_n()
    
    }
  
  return(tbl)
}

# data %>%
#   select(provide_wls.factor:appreciation.factor) %>%
#   make_tbl_likert(., labels, title = "Questions Foo!!!")


##################### FENCE ####################################################






#4. Free text ------------------------------------------------------------------
# FREE RESPONSE TABLE FUNCTION -------------------------------------------------
# Function that creates a table, will assign to the global environment
generate_fr_tab <- function(var, name){
  # var => is the variable associated with the question
  # name => the output to assign the table to
  
  # Some practices may not have any responses to the free response questions
  # The function begins by converting blanks ("") to NAs. Then the function
  # counts the number of cells that are NOT NAs. If that sum is greater than or 
  # equal to 1, then there are free responses and the rest of the function
  # proceeds to generate a table of those responses.
  temp[var][temp[var] == ""] <- NA
  if(sum(!is.na(temp[var])) >= 1){
    tab <- temp %>%
      select(all_of(var)) %>%
      filter(!!rlang::sym(var) != "") %>%
      tbl_summary() %>%
      remove_row_type(type = "header") %>%
      modify_header(label = label(temp[var])) %>%
      modify_column_hide(stat_0)
    assign(name, tab, envir = .GlobalEnv)
  }
}

# Response pattern -------------------------------------------------------------
# Response pattern to determine how many participants have both pre and post
# values
response_pattern <- function(data, column){
  tbl <- data %>%
    select(record_id, !!rlang::sym(column), redcap_event_name.factor) %>%
    drop_na(!!rlang::sym(column)) %>%
    group_by(record_id) %>%
    count() %>%
    ungroup() %>%
    mutate(n = 
             recode(n, `1` = "Pre or Post Only", `2` = "Both Pre and Post")) %>%
    select(-record_id) %>%
    rename("Number of responses per participant" = n) %>%
    tbl_summary() %>%
    modify_footnote(all_stat_cols() ~ "n (%); excludes participants with missing data at both baseline and followup.")
    
  return(tbl)
}


#5. Race ethnicity -------------------------------------------------------------
# Requires binary indicator variables
# Should require BSTFUN add_variable grouping
# How to make the inputs into a parameter


#6. Order the race and ethnicity functions




#7. Rowwise Percentages
# Requires a vector of dichotomous variables and a string for the title
# create_table <- function(vars, title){
#   
#   # Create individual tables for each variable
#   if (sum(grepl("Needs Interpreter", vars)) == 1){
#     tables <- map(outcomes, ~ data %>%
#                     filter(!!rlang::sym(.x) == "IN_NUMERATOR" |
#                              !!rlang::sym(.x) == "IN_DENOMINATOR") %>%
#                     select(all_of(c(.x, vars))) %>%
#                     tbl_summary(by = .x,
#                                 type = everything() ~ "categorical",
#                                 percent = "row",
#                                 statistic = all_categorical() ~ "{n}/{N} ({p}%)") %>%
#                     modify_column_hide(columns = "stat_1") %>%
#                     modify_header(stat_2 = str_c("**", .x, "**")))
#   } else {
#     tables <- map(outcomes, ~ data %>%
#                     filter(!!rlang::sym(.x) == "IN_NUMERATOR" |
#                              !!rlang::sym(.x) == "IN_DENOMINATOR") %>%
#                     select(all_of(c(.x, vars))) %>%
#                     tbl_summary(by = .x,
#                                 missing = "no",
#                                 #type = everything() ~ "categorical",
#                                 percent = "row",
#                                 statistic = all_categorical() ~ "{n}/{N} ({p}%)") %>%
#                     modify_column_hide(columns = "stat_1") %>%
#                     modify_header(stat_2 = str_c("**", .x, "**")))
#   }
#   
#   
#   # Merge all of the individual tables into one
#   table <- tbl_merge(list(tables[[1]],
#                           tables[[2]],
#                           tables[[3]],
#                           tables[[4]],
#                           tables[[5]],
#                           tables[[6]],
#                           tables[[7]],
#                           tables[[8]],
#                           tables[[9]])
#   )
#   
#   # Set the title of the final output table
#   title <- str_c("**Health Outcomes by ", title, "**")
#   
#   # Modify the header names
#   table %>%
#     modify_header(label = "",
#                   stat_2_1 = "**HbA1c < 8%**",
#                   stat_2_2 = "**HbA1c last 6 months**",
#                   stat_2_3 = "**Nephropathy Screen**",
#                   stat_2_4 = "**Statin Use**",
#                   stat_2_5 = "**Blood Pressure < 140/90**",
#                   stat_2_6 = "**Breast Cancer Screen**",
#                   stat_2_7 = "**Cervical Cancer Screen**",
#                   stat_2_8 = "**Colorectal Cancer Screen**",
#                   stat_2_9 = "**Depression Screen**"
#     ) %>% 
#     modify_spanning_header(everything() ~ NA_character_) %>%
#     as_gt() %>%
#     tab_header(title = md(title))
# }


####################### CHARTS ################################################


