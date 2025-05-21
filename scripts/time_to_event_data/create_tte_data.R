# /////////////////////////////////////////////////////////////////////////////
# Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine
# 05/20/2025
#
# HSQ - Create time to event (tte) data set 
#
# Description -  This script is designed to download the HSQ SMS RedCap data to
# create the time to event analysis data set.
#
# /////////////////////////////////////////////////////////////////////////////

pacman::p_load(here,
               tidyverse,
               magrittr,
               gtsummary,
               Hmisc, 
               gt,
               install = FALSE)

# Loads the function that updates data from the hsq participant management project
# source(here("Helpers-Stay-Quit", "03 Code/functions/get_hsq_data.R"))
source(here("scripts/functions/get_hsq_data.R" ))


# Load updated 0, 3, 6, 9, and 12 month survey data ---------------------------
# This will include rows where participants were not randomized to a treatment
# and represents the entire data set in the primary HSQ RedCap project. This 
# data will be used to capture participant covariate information 
data <- get_hsq_data()

# Define get_redcap_report function which will retrieve a report from a given
# RedCap project according to the report_id (assigned in RedCap)
get_redcap_report <- function (token, report_id, labels = "raw"){
  url <- "https://redcap.ucdenver.edu/api/"

 formData <- list(token = token, content = "report", format = "csv",
      report_id = report_id, csvDelimiter = "", rawOrLabel = labels,
      rawOrLabelHeaders = "raw", exportCheckboxLabel = "false",
      returnFormat = "csv")

  response <- httr::POST(url, body = formData, encode = "form")

  result <- httr::content(response)

  return(result)
}


# Retrieve SMS data from HSQ SMS Survey report --------------------------------
sms <- get_redcap_report(token = Sys.getenv("HSQ_sms"),
                                   report_id = "117325")

# Process the sms data set, by creating rows for which patients did not submit
# an sms survey, in order to calculate the actual number of missing data
sms %<>%
  # Introduce rows with missing values for each possible sms week to create a
  # complete data set
  complete(record_id, redcap_event_name) %>% 
  group_by(record_id) %>%
  fill(randomization_dtd, .direction = "downup") %>%
  fill(sms_strt_dtd, .direction = "downup") %>%
  fill(hsqid, .direction = "downup") %>%
  fill(patientend_dtd, .direction = "downup") %>%
  ungroup() %>%
  # this row contains housekeeping data and is not of interest, remove
  filter(redcap_event_name != "week0_arm_1") %>% 
  mutate(redcap_event_name = str_replace_all(redcap_event_name,
                                             c("week1_" = "week01_",
                                               "week2_" = "week02_",
                                               "week3_" = "week03_",
                                               "week4_" = "week04_",
                                               "week5_" = "week05_",
                                               "week6_" = "week06_",
                                               "week7_" = "week07_",
                                               "week8_" = "week08_",
                                               "week9_" = "week09_"))) %>%
  mutate(redcap_event_name = str_replace(redcap_event_name, "_arm_1", "")) %>%
  mutate(weeks_since_rand = round(difftime(Sys.Date(), as.Date(randomization_dtd), unit = "weeks"))) %>%
  mutate(week = as.numeric(str_replace(redcap_event_name, "week", ""))) %>%
  arrange(record_id, week) %>%
  select(-redcap_survey_identifier) %>%
  mutate(sms_strt_dtd = as.Date(sms_strt_dtd),
         sms_survey_timestamp = as.Date(sms_survey_timestamp))



# Create a long data set of time to event (tte) data
# filter out the missing values for which patients should not have any data
# For example, if someone has not been in the study long enough to get a 6 week sms,
# then remove any rows beyond week 6, because they will all be missing, because that
# patient has not received a 6-week sms
tte_data  <- sms %>% 
  select(record_id, week, smoke1, weeks_since_rand, sms_survey_timestamp, randomization_dtd) %>%
  mutate(weeks_since_rand = as.numeric(weeks_since_rand)) %>%
  filter(week < weeks_since_rand) %>%
  rename(event = smoke1) %>%
  mutate(sms_survey_timestamp = as.POSIXct(sms_survey_timestamp)) %>%
  mutate(across(c(sms_survey_timestamp), ~ as.Date(.))) %>%
  mutate(time_to_event = as.numeric(sms_survey_timestamp - randomization_dtd)) %>%
  mutate(previous_relapse = lag(time_to_event)) %>%
  mutate(previous_relapse = ifelse(lag(event) == 1, previous_relapse, NA))


# Write out a sample data set for Jun to review
tte_data %>%
  head(1000) %>%
  write_csv(here("scripts", "time_to_event_data", "hsq_time_to_even_sample.csv"), na = "")