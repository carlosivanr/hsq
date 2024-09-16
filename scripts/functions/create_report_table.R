# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Create report table function

# Description:
# This function generates a table to display the number of incomplete, 
# unverified, or completed survey responses in the PATHWEIGH practice member
# survey.

# The function creates two tables that are then merged together to show the 
# percent across rows for each role and the overall percent in reference to the 
# clinic.

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_report_tbl <- function(clinic){

  tbl_row_percent <-
    data %>%
    filter(practice == clinic) %>%
    select(pm_role.factor, pathweigh_practice_member_survey_complete.factor) %>%
    rename("Role" = pm_role.factor) %>%
    tbl_summary(by = pathweigh_practice_member_survey_complete.factor,
                percent = "row")

  tbl_col_percent <-
    data %>%
    filter(practice == clinic) %>%
    select(pm_role.factor) %>%
    rename("Role" = pm_role.factor) %>%
    tbl_summary() %>%
    modify_header(all_stat_cols() ~ "**Overall**, N = {N}")

  tbl_final <-
    tbl_merge(list(tbl_col_percent, tbl_row_percent)) %>%
    modify_spanning_header(everything() ~ NA)

  tbl_final <-
    tbl_final %>%
    modify_header(label = str_c("**", " ", "**")) #clinic can be placed here
  return(tbl_final)
}