################################################################################
# Carlos Rodriguez, PhD. CU Anschutz, Dept. of Fam. Medicine
# Helpers stay quit - combine sms and quarterly survey to create a variable for
# smoking status at 3, 6, 9, and 12 months.

# fu_smoke1 in quarterly survey asks, Have you smoked cigarettes in the past 30 
# days

# smoke1 from the sms surveys asks Have you smoked or vaped in the past 7 days

# sms weeks 11, 12 and 13 are then converted to the 3month time point

# sms weeks 23 and 24 are converted to the 6month time point. Does not include
# week 26 because week 24 switches to a 2-week cadence

# sms weeks 34, 36, and 38 are converted to 9month time point

# sms weeks 46 and 48 get converted to the 12 month timepoint. week 48 is the 
# last week in the study.

# 3month variable corresponds to smoking in the last 30 days, or the 
# two weeks prior to 3month timep pint and one week after the 3month time point.

# 6month variable corresponds to smoking in the last 30 days, or the 
# two weeks prior to the 6 month timepoint.

# 9month variable correspons to smoking in the last 30 days, or the two weeks
# prior to the 9month timepoint, or 4 weeks after the 9month time point.

# 12month variable correspons to smoking in the last 30 days, or the two weeks
# prior to the 12month timepoint


################################################################################


# Load packages
pacman::p_load(here,
               tidyverse,
               magrittr,
               gtsummary,
               Hmisc, 
               gt,
               install = FALSE)


# Quarterly Longitudinal Data
source(here("scripts/functions/get_hsq_data.R" ))


# Load updated 0, 3, 6, 9, and 12 month survey data
# This will include rows where participants were not randomized to a treatment
# and represents the entire data set in the primary HSQ RedCap project
data <- get_hsq_data()

# Create a combined list of data frames that have been filtered by a length of
# time. Patients who were recently randomized, may have baseline survey data 
# available, but may not have been in the study long enough to receive a follow
# up survey. Including these patients in a count for a follow up survey would 
# increase the denominator to the total number of participants and inflate the 
# number of missing values, and produce aberrant proportions when tabulating.

# To prevent counting patients who have not been in the study long enough to
# receive a followup survey at a given time point, individual data sets are 
# filtered according to the number of days since randomization + a 14 day period
# to complete a survey. For example, to filter the 3 month data, patients must 
# have been enrolled in the study for at least 90 + 14 or 104 days.

# Data for the baseline survey are only filter to those that have been 
# randomized to one of the treatment groups.

# Filter data and select columns of interest
data %<>%
  select(hsqid, event_name, starts_with("fu_smoke")) %>%
  filter(!event_name %in% "0mo")

# ////////////////////////////////////////////////////////////////////////////
#                     Retrieve SMS data from RedCap
# /////////////////////////////////////////////////////////////////////////////

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

# Retrieve SMS data from HSQ SMS Survey report
sms <- get_redcap_report(token = Sys.getenv("HSQ_sms"), report_id = "117325")

# If warning arises, use problems() to investigate.
# warning arises because sms_survey_timestamp contains "[not completed]"
# problems(sms)

# /////////////////////////////////////////////////////////////////////////////
#                          Clean and create variables
# /////////////////////////////////////////////////////////////////////////////
# Process the sms data set, by creating rows for which patients did not submit
# an sms survey, in order to calculate the actual number of missing data points
sms %<>%
  # Introduce rows with missing values for each possible sms week
  complete(record_id, redcap_event_name) %>% 
  group_by(record_id) %>%
  fill(
    randomization_dtd, 
    sms_strt_dtd, 
    hsqid, 
    patientend_dtd, 
    .direction = "downup") %>%
  ungroup() %>%
  # This row contains housekeeping data and is not of interest
  filter(redcap_event_name != "week0_arm_1") %>% 
  # introduce a 2-digit number for sorting
  mutate(redcap_event_name = str_replace_all(
    redcap_event_name,
    c("week1_" = "week01_",
      "week2_" = "week02_",
      "week3_" = "week03_",
      "week4_" = "week04_",
      "week5_" = "week05_",
      "week6_" = "week06_",
      "week7_" = "week07_",
      "week8_" = "week08_",
      "week9_" = "week09_"))) %>% 
  # remove the arm 1 string
  mutate(redcap_event_name = str_replace(redcap_event_name, "_arm_1", "")) %>% 
  mutate(
    across(c(sms_strt_dtd, sms_survey_timestamp, randomization_dtd), 
    ~ as.Date(.x))) %>%
  mutate(weeks_since_rand = 
    round(difftime(Sys.Date(), randomization_dtd, unit = "weeks"))) %>%
  mutate(
    week = as.numeric(str_replace(redcap_event_name, "week", "")),
    first_half = ifelse(week > 24, 0, 1)) %>%
  arrange(record_id, week) %>%
  mutate(row_id = str_c(record_id, redcap_event_name)) %>%
  select(-redcap_survey_identifier)


# set 3 month to 12 weeks, 6 month to 24 weeks, 9 month to 36 weeks, 12 month to
# 48 weeks
sms %<>%
  filter(week %in% c(11,12,13, 23,24, 34,36,38, 46,48))

# sms %>%
#   select(hsqid, randomization_dtd, sms_strt_dtd, weeks_sinc_rand, smoke1)


sms %<>%
  mutate(
    redcap_event_name = ifelse(redcap_event_name %in% c("week11", "week12", "week13"), "3mo",
    ifelse(redcap_event_name %in% c("week23", "week24"), "6mo",
    ifelse(redcap_event_name %in% c("week34", "week36", "week38"), "9mo", "12mo")))
  ) %>%
  group_by(hsqid, redcap_event_name) %>%
  summarise(relapse = sum(smoke1, rm.na = TRUE), .groups = "drop") %>%
  mutate(relapse = ifelse(is.na(relapse),0, 1))



data %<>%
  filter(hsqid %in% sms$hsqid) %>%
  mutate(relapse = ifelse(fu_smoke1 == "Yes", 1, ifelse(fu_smoke1 == "No", 0, fu_smoke1))) %>%
  select(hsqid, event_name, relapse) %>%
  mutate(relapse = ifelse(is.na(relapse), 0, relapse))


# Create a stacked data frame of the 
data_out <- data %>% 
  mutate(relapse = as.numeric(relapse)) %>%
  bind_rows(
    sms %>%
    rename(event_name = redcap_event_name)
) %>%
  arrange(hsqid, event_name) %>%
  group_by(hsqid, event_name) %>%
  summarise(relapse = sum(relapse), .groups = "drop") %>%
  mutate(relapse = ifelse(relapse > 0 , 1, relapse))

data_out %>% pull(relapse) %>%table(., useNA = "ifany")


data_out %<>%
  mutate(event_name = ifelse(!event_name %in% "12mo", str_c("0", event_name), event_name)) %>%
  arrange(hsqid, event_name)


# ///////////////////////////////////////////////////////////////////////////
#                          Output to file
# ///////////////////////////////////////////////////////////////////////////
# Before outputing to .csv Convert dates to strings and save with na = ''
# to make compatible with SAS.
data_out %>%
  mutate_if(is.Date, as.character) %>%
  write_csv(here("analyses_sas", "data", "combined_sms_quarterly_status.csv"), na = "")

# Identify the first relapse
# relapse_ids <- data_out %>% filter(relapse == 1) %>% pull(hsqid) %>% unique()

# # Should be 812 participants
# relapsed <- data_out %>%
#   filter(
#     hsqid %in% relapse_ids,
#     relapse == 1) %>% 
#   group_by(hsqid) %>%
#   slice_head() %>%
#   ungroup()

# # Should be 128 participants
# not_relapsed <- 
#   data_out %>%
#   filter(!hsqid %in% relapsed$hsqid) %>%
#   group_by(hsqid) %>%
#   slice_tail() %>%
#   ungroup()