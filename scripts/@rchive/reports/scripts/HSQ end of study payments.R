# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. CU Anschutz Dept. of Fam. Medicine
# HSQ - End of study payments

# the purpose of this script is to determine which participants need to be paid
# at the end of the helpers stay quit study

# This script updates a master file for end of study participant payment data
# 1. pull redcap data from 2 separate projects.
#   a)The first is the sms project to check who has completed all sms surveys
#   b)The second is the main project data to check who has completed the base-
#     line, 3-month, 6-month, 9-month, and 12-month surveys
# 2. Pull master data sheet from egnyte
#   This data is where information of whether or not a participant has been
#   paid is updated along with whether or not they have been loss to followup
# 3. determine which records are new/not in master data sheet and add their
#     rows of information, and fixes NAs
# 4. Write the updated master file
# 6. Write a separate focused file that only has rows where the loss to follow
#     up == 0 and paid == 0.

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load libraries ---------------------------------------------------------------
pacman::p_load(REDCapR,
               here,
               tidyverse,
               magrittr, install = FALSE)

# Load redcap data ------------------------------------------------------------

# Download/refresh data
sms_data <- REDCapR::redcap_read(
      redcap_uri = "https://redcap.ucdenver.edu/api/",
      token = Sys.getenv("HSQ_sms")
      )$data

# *** Could use a report in redcap with week0 rows only


# Get the sms survey count data ------------------------------------------------
# Purpose is to determine which participants have all 36 sms payments for their
# surveys, the number of remaining sms payments out of the 5 possible payments
# then determine who has completed the sudy by looking at how long they have
# been in the study.
update_sms <-
  sms_data %>%
  filter(redcap_event_name == "week0_arm_1") %>%
  select(record_id, hsqid, redcap_event_name, randomization_dtd, smssurvey_count) %>%
  mutate(remaining_sms_payments = smssurvey_count %% 5,
         all_36_sms_complete = ifelse(smssurvey_count == 36, 1, 0),
         randomization_dtd = as.Date(randomization_dtd),
         estimated_end_date = randomization_dtd + 380,
         # completed_study = ifelse(randomization_dtd >= Sys.Date(), 1, 0), #CR. modified 1/19/2024
         completed_study = ifelse(Sys.Date() >= as.Date(estimated_end_date), 1, 0)
         )


# Determine those with all of the three six and nine month surveys -------------
# These are a separate set of surveys from the sms surveys which are more for
# tracking smoking status.

# Download/refresh 3,6,9,12 month data
alter_data <- REDCapR::redcap_read(
      redcap_uri = "https://redcap.ucdenver.edu/api/",
      token = Sys.getenv("HSQ_api")
      )$data

# alter survey complete
update_alter <- alter_data %>%
  select(hsqid,
         survey_3_month_complete,
         survey_6_month_complete,
         survey_9_month_complete,
         survey_12_month_complete) %>%
  filter(hsqid %in% update_sms$hsqid) %>%
  drop_na(hsqid) %>%
  mutate(across(survey_3_month_complete:survey_12_month_complete, ~recode(., `2` = 1))) %>%
  mutate(n_3_6_9_12_mo_surveys_complete = (select(., survey_3_month_complete:survey_12_month_complete) %>% rowSums(., na.rm = TRUE)),
         all_3_6_9_12_mo_surveys_complete = ifelse(n_3_6_9_12_mo_surveys_complete == 4, 1, 0)) %>%
  select(-n_3_6_9_12_mo_surveys_complete)

# Read in master file ----------------------------------------------------------
# Set file path to read in the master file
file <- "Z:/Shared/DFM/HSQ_Shared/Reporting/End of Study Payments/end_of_study_payments_master.csv"

# Read in the egnyte master
master <- read_csv(file, show_col_types = F)

# Save a copy of master file to @archive for back up
write_csv(master,
          str_c("Z:/Shared/DFM/HSQ_Shared/Reporting/End of Study Payments/@rchive/end_of_study_payments_",
                Sys.Date(),
                ".csv"))

# get col names to index and order the col names of the data to update
col_names <- names(master)

# remove the last end of payment column
col_names <- col_names[!col_names %in% c("end_of_study_payment_complete","loss_to_followup")]


# Merge in new data ------------------------------------------------------------
# merge the sms and the 3,6,9,12-month survey data redcap data
update <- left_join(update_sms, update_alter, by = "hsqid") %>%
  select(all_of(col_names))

# Merge the refreshed redcap data with the master end of study payment complete
# data
out <-
  left_join(update, (master %>%select(hsqid, end_of_study_payment_complete, loss_to_followup)), by = "hsqid") %>%
  mutate(end_of_study_payment_complete = ifelse(is.na(end_of_study_payment_complete), 0, end_of_study_payment_complete)) %>%
  mutate(loss_to_followup = ifelse(is.na(loss_to_followup), 0, loss_to_followup))



# Write out to .csv, -----------------------------------------------------------
# n.b. overwrites master file
write_csv(out, here(file))

focused <- out %>%
  filter(loss_to_followup == 0, end_of_study_payment_complete == 0) %>%
  select(-end_of_study_payment_complete, -loss_to_followup)

focused_file <- "Z:/Shared/DFM/HSQ_Shared/Reporting/End of Study Payments/focused_output.csv"
write_csv(focused, focused_file)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# # This section was the code written to create a master file
# #  !!!!!!!!!!!!!!!!!! DO NOT RUN THIS SECTION !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# # Create the master file -------------------------------------------------------
#
# # Load the get_redcap_data() function
# source(here("scripts", "functions", "get_redcap_data.R"))
#
# # Set the project ID to the hsq sms survey project
# project_id <- 26962
#
# # Download/refresh data
# data <- get_redcap_data(project_id)
#
# # Create data frame for the sms survey data
# sms <- data %>%
#   filter(redcap_event_name == "week0_arm_1") %>%
#   select(record_id, hsqid, redcap_event_name, randomization_dtd, smssurvey_count) %>%
#   mutate(remaining_sms_payments = smssurvey_count %% 5,
#          all_36_sms_complete = ifelse(smssurvey_count == 36, 1, 0),
#          randomization_dtd = as.Date(randomization_dtd),
#          estimated_end_date = randomization_dtd + 380,
#          completed_study = 0,
#          end_of_study_payment_complete = 0)
#
#
# # Set the project ID to the hsq participant management project
# project_id <- 25710
#
# # Download/refresh data
# data <- get_redcap_data(project_id)
#
# # alter survey complete
# alter <- data %>%
#   select(hsqid,
#          survey_3_month_complete,
#          survey_6_month_complete,
#          survey_9_month_complete,
#          survey_12_month_complete) %>%
#   filter(hsqid %in% sms$hsqid) %>%
#   drop_na(hsqid) %>%
#   mutate(across(survey_3_month_complete:survey_12_month_complete, ~recode(., `2` = 1))) %>%
#   mutate(n_3_6_9_12_mo_surveys_complete = (select(., survey_3_month_complete:survey_12_month_complete) %>% rowSums(., na.rm = TRUE)),
#          all_3_6_9_12_mo_surveys_complete = ifelse(n_3_6_9_12_mo_surveys_complete == 4, 1, 0)) %>%
#   select(-n_3_6_9_12_mo_surveys_complete)
#
# seed_df <- left_join(sms, alter, by = "hsqid") %>%
#   select(record_id:randomization_dtd,
#          randomization_dtd:completed_study,
#          survey_3_month_complete:survey_12_month_complete,
#          smssurvey_count,
#          remaining_sms_payments,
#          all_36_sms_complete,
#          all_3_6_9_12_mo_surveys_complete,
#          end_of_study_payment_complete,
#          -redcap_event_name)
#
#
# # Write out to .csv,
# write_csv(seed_df, here("end_of_study_payments.csv"))
