################################################################################
# Carlos Rodriguez, PhD. CU Anschutz, Dept. of Fam. Medicine
# Helpers stay quit - prepare sms data
#
# This script is designed to prepare the sms data in the HSQ study.
# SMS data refers to the weekly and bi-weekly sms prompted surveys collected in 
# a separate redcap project. 
#
# SMS data collects smoking status, helping conversations, and cessation 
# fatigue. Smoking status is used to prepare time to event data (time to first
# relapse), number of quit attempts, and the duration of each quit attempt.
#
# Outputs a .csv file for analysis in SAS
################################################################################

pacman::p_load(here,
               tidyverse,
               magrittr,
               gtsummary,
               Hmisc, 
               gt,
               install = FALSE)


# /////////////////////////////////////////////////////////////////////////////
#                      Retrieve data from RedCap
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


# Estimate survey time stamps -------------------------------------------------
# There are rows for which a participant entered a smoke1 or smoke2 status, 
# but did not submit their survey, so a time stamp was not collected. 
# Timestamps can be estimate these from the sms strt date and the week number 
# since we know, most people submit their sms survey the same day the sms 
# invite is sent.

nrows_to_est <- sms %>%
  select(record_id, week, smoke1, randomization_dtd, sms_survey_timestamp)%>%
  filter(!is.na(smoke1), is.na(sms_survey_timestamp)) %>%
  nrow()

row_ids_to_estimate <- sms %>%
  filter((!is.na(smoke1) | !is.na(smoke2)) & is.na(sms_survey_timestamp)) %>%
  pull(row_id)

estimated_time_stamps <- sms %>%
  filter(row_id %in% row_ids_to_estimate) %>%
  mutate(sms_survey_timestamp = sms_strt_dtd + ((week-1) * 7 + 0))

sms <- bind_rows(
    sms %>% filter(!row_id %in% estimated_time_stamps$row_id),
    estimated_time_stamps)

# At least one relapse --------------------------------------------------------
# Create a vector of ids of participants with at least one relapse
at_least_1_relapse <- sms %>%
  filter(smoke1 == 1) %>%
  pull(record_id)


sms %<>%
  mutate(
    at_least_1_relapse = ifelse(record_id %in% at_least_1_relapse, 1, 0))

rm(at_least_1_relapse)

# Tabulate the number of relapsers vs non-relapsers
sms %>%
  group_by(record_id) %>%
  slice_head() %>%
  ungroup() %>%
  select(at_least_1_relapse) %>%
  tbl_summary() %>%
  as_tibble()


# /////////////////////////////////////////////////////////////////////////////
#                     Number and duration of relapses
# /////////////////////////////////////////////////////////////////////////////

# Create an index of the row ids where the first relapse occured
first_relapse_ids <- sms %>% 
  filter(at_least_1_relapse == 1, smoke1 == 1) %>%
  arrange(record_id, redcap_event_name) %>%
  group_by(record_id) %>%
  slice_head() %>%
  ungroup() %>%
  pull(row_id)

# Create a variable to indicated which sms text survey the participant 
# indicated their first relapse
sms %<>%
  mutate(first_relapse = ifelse(row_id %in% first_relapse_ids, 1, 0))

# Duration of first relapse = time to subsequent quit attempt
# This may not be needed 
# first_relapse_duration <- sms %>%
#   filter(record_id %in% at_least_1_relapse, smoke1 == 1) %>%
#   group_by(record_id) %>%
#   mutate(first_relapse_dur = lag(sms_survey_timestamp)) %>%
#   ungroup() %>%
#   select(row_id, first_relapse_dur) %>%
#   drop_na()

# sms %<>%
#   left_join(first_relapse_duration, by = "row_id")


# 5068 rows that have a smoke1 status
sms %>%
  filter(smoke1 == 1) %>% nrow()

# 4870 rows that have smoke status AND smoke_days value
# amounts to about 96% of the values.
sms %>%
  filter(smoke1 == 1, !is.na(smoke_days)) %>% nrow()

# Relapse duration ------------------------------------------------------------
# There are two possible ways to count the number of relapses and their duration
# The first is to use the value provided in the smoke_days which might be more
# accurate, but there's some data loss (4%). The second way is to estimate 
# based off of weeks, which would inflate the duration, but there's less data 
# loss.

# Actual duration of a relapse from smoking or vaping using the smoke_days 
# variable. Could also be thought of as dwell time
duration_act <- 
  sms %>%
  filter(at_least_1_relapse == 1) %>%
  select(record_id, redcap_event_name, smoke1, first_relapse, smoke_days) %>%
  mutate(run_id = consecutive_id(smoke1)) %>%
  filter(smoke1 == 1) %>%
  group_by(record_id, run_id) %>%
  summarise(
    duration_act = sum(smoke_days, na.rm = TRUE),
    .groups = "drop") %>%
  group_by(record_id) %>%
  summarise(
    n_relapses = n(),
    total_duration_act = sum(duration_act)
  )

# Version using survey time stamps to estimate the number of days the relapse 
# lasted. However, the start date at the beginning of a run would be estimated
# to have lasted 7 days. Would need to add 7 days to each run in the first
# half and 14 days to each run in the second half of the study. This would also
# overestimate the duration of relapse, because the mean of all smoke days is
# 5.06 in the 2nd half and 4.96 in the first half. Many just smoke a few days
# if relapsed
# duration_est <- 
#   sms %>%
#   filter(record_id %in% at_least_1_relapse, record_id == 1) %>% # *** filtered to record id 1 to demonstrate
#   select(record_id, redcap_event_name, smoke1, first_relapse, sms_survey_timestamp) %>%
#   mutate(run_id = consecutive_id(smoke1)) %>%
#   filter(smoke1 == 1) %>%
#   group_by(record_id, run_id) %>%
#   summarise(
#     start = min(sms_survey_timestamp),
#     stop = max(sms_survey_timestamp),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     duration_est = as.numeric(stop - start),
#     duration_est = ifelse(duration_est == 0, 7, duration_est)) %>%
#   group_by(record_id) %>%
#   summarise(
#     n_relapses = n(),
#     total_duration_est = sum(duration_est)
#   )

# Subsets to those that relapse
plt_n_relapse <- duration_act %>%
  ggplot(aes(x = n_relapses)) +
  geom_histogram(bins = 25)

# Summarise n_relapses, duration, and smoke days per relapse
# Only subsets to those that relapsed
duration_act %>%
  mutate(n_smoke_days_per_relapse = total_duration_act / n_relapses) %>%
  summarise(
    mean_n_relapses = mean(n_relapses),
    mean_duration_days = mean(total_duration_act),
    mean_smoke_days_per_relapse = mean(n_smoke_days_per_relapse)
  )


# /////////////////////////////////////////////////////////////////////////////
#                     Time to subsequent quit attempt
# /////////////////////////////////////////////////////////////////////////////
# How soon after relapse do they take to quit again?
first_relapse_week <- sms %>%
  filter(at_least_1_relapse ==1 , first_relapse == 1) %>%
  select(record_id, week) %>%
  rename(first_relapse_week = week)


time_to_2nd_attempt <- 
  sms %>%
  filter(at_least_1_relapse == 1) %>%
  left_join(first_relapse_week, by = "record_id") %>%
  select(record_id, redcap_event_name, smoke1, first_relapse, week, first_relapse_week, first_half) %>%
  filter(week >= first_relapse_week) %>%
  group_by(record_id) %>%
  mutate(run_id = consecutive_id(smoke1)) %>%
  ungroup() %>%
  filter(run_id == 1) %>%
  mutate(t_to_2nd_attempt = ifelse(first_half == 1, 7, 14)) %>%
  group_by(record_id, run_id) %>%
  summarise(
    t_to_2nd_attempt = sum(t_to_2nd_attempt, na.rm = TRUE),
    .groups = "drop") %>%
  select(record_id, t_to_2nd_attempt)


# /////////////////////////////////////////////////////////////////////////////
#                              Quit Attempts
# /////////////////////////////////////////////////////////////////////////////
# Number of quit attempts and duration of each quit attempt 
# If someone reports 0 to smoke1, it means that they didn't smoke or vape in the
# last 7 days. Quit attempts are only counted for those that had at least one
# relapse, otherwise the quit attemps are 0 for those that did not relapse. This
# version uses NAs in smoke1 to break up sequences. One alternative is to carry
# the last observation forward to maintain continuity in responses.
quit_attempts <- 
  sms %>%
  filter(at_least_1_relapse == 1) %>%
  select(record_id, redcap_event_name, smoke1, first_relapse, week, first_half) %>%
  mutate(run_id = consecutive_id(smoke1)) %>%
  filter(smoke1 == 0) %>%
  mutate(quit_days = ifelse(first_half == 1, 7, 14)) %>%
  group_by(record_id, run_id) %>%
  summarise(
    duration_quit = sum(quit_days, na.rm = TRUE),
    .groups = "drop") %>%
  group_by(record_id) %>%
  summarise(
    n_quit_attempts = n(),
    total_quit_dur = sum(duration_quit))

# Check those that stayed quit, and see if they are in the quit_attempts df
# View(sms %>%
#   filter(
#     record_id %in% (sms %>%
#                     group_by(record_id) %>%
#                     summarise(n_dist = n_distinct(smoke1), .groups = "drop") %>%
#                     filter(n_dist == 1) %>%
#                     pull(record_id))
#   )
# )


# ///////////////////////////////////////////////////////////////////////////
#                         Time to first relapse
# ///////////////////////////////////////////////////////////////////////////
# Time it takes to submit a survey. Most participants submit their sms survey
# the same day they are invited to submit. 
t_submit <- 0

# Create an intermediary data frame where only patients that relapsed are 
# retained, sort by date, then select the first available row to capture the 
# date of the first relapse. Time stamps are then converted to dttm format as an
# intermediary step to convert to as.Date(). The randomization dates are then 
# subtracted from the time stamp to calculate the time to the first relapse.
first_relapse <- 
  sms %>%
  filter(smoke1 == 1) %>% 
  mutate(sms_survey_timestamp = if_else(is.na(sms_survey_timestamp) & !is.na(smoke1), 
                                        (sms_strt_dtd + ((week-1) * 7 + t_submit)), sms_survey_timestamp)) %>%
  arrange(record_id, sms_strt_dtd) %>%
  group_by(record_id) %>%
  slice_head() %>%
  ungroup() %>%
  mutate(sms_survey_timestamp = as.POSIXct(sms_survey_timestamp)) %>%
  mutate(across(c(sms_strt_dtd, sms_survey_timestamp), ~ as.Date(.))) %>%
  mutate(time_to_relapse = sms_survey_timestamp - randomization_dtd) %>%
  select(record_id, time_to_relapse)

# Includes partially completed surveys, by estimating a sms_survey_timestamp as
# the sum of the sms start date and the week-1 number * 7 days plus 0 to 
# estimate the date of the last observation, otherwise the last observation is 
# the last fully complete response, which could be week 26 when they have a
# partial response for week 28 for example.
last_observation <-
  sms %>% 
  mutate(
    sms_survey_timestamp = if_else(
      is.na(sms_survey_timestamp) & !is.na(smoke1), 
      (sms_strt_dtd + ((week-1) * 7 + t_submit)), 
      sms_survey_timestamp)) %>%
  arrange(record_id, week) %>%
  drop_na(smoke1) %>%
  group_by(record_id) %>%
  slice_tail() %>%
  ungroup() %>%
  mutate(last_obs_dtd = as.POSIXct(sms_survey_timestamp)) %>%
  mutate(across(c(sms_strt_dtd, sms_survey_timestamp), ~ as.Date(.))) %>%
  mutate(time_to_last_obs = sms_survey_timestamp - randomization_dtd) %>%
  select(record_id, time_to_last_obs)

# Create a data set that displays the number of days to the first relapse
# One row per patient. In this data frame, those with missing time represent
# true missing values. 
t_to_relapse <- 
  sms %>%
  select(record_id, hsqid, randomization_dtd, sms_strt_dtd) %>%
  group_by(record_id) %>%
  slice_head() %>%
  ungroup() %>%
  left_join(., first_relapse, by = "record_id") %>%
  left_join(., last_observation, by = "record_id") %>%
  mutate(event = ifelse(is.na(time_to_relapse), 0, 1),
         time = ifelse(is.na(time_to_relapse), time_to_last_obs, time_to_relapse),
         status = ifelse(event == 1, "relapse", "abstain")) %>%
  mutate(across(c(event, status), ~ifelse(is.na(time), NA, .)))

# ///////////////////////////////////////////////////////////////////////////
#                     Number of helping conversations
# ///////////////////////////////////////////////////////////////////////////
n_hcs <- sms %>%
  group_by(record_id) %>%
  summarise(
    n_hcs = sum(as.numeric(help1), na.rm = TRUE)
  )

# ///////////////////////////////////////////////////////////////////////////
#                     Average Cessation Fatigue
# ///////////////////////////////////////////////////////////////////////////
plt_avg_cessation_fatigue_by_week <- sms %>%
  select(record_id, week, cessation_fatigue) %>%
  drop_na(cessation_fatigue) %>%
  group_by(week) %>%
  summarise(
    avg_cf = mean(cessation_fatigue), .groups = "drop"
  ) %>%
  ggplot(., aes(x = week, y = avg_cf)) +
    geom_point() +
    geom_line() +
  lims(y = c(1, 5)) +
  theme_minimal()

# Randomly sample a handful of participants and plot their cessation fatique
# over time.These data would be good for a multilevel model of an ordinal
# outcome
sms %>%
  select(record_id, week, cessation_fatigue) %>%
  drop_na(cessation_fatigue) %>%
  filter(record_id %in% (slice_sample(duration_act, n = 5) %>% pull(record_id))) %>%
  mutate(record_id = factor(record_id)) %>%
  ggplot(., aes(x = week, y = cessation_fatigue, group = record_id, color = record_id)) +
    geom_point() +
    geom_line() +
  lims(y = c(1, 5)) +
  theme_minimal()

# boxplot of each participants average cessation fatigue score
# 
sms %>%
  select(record_id, week, cessation_fatigue) %>%
  drop_na(cessation_fatigue) %>%
  group_by(record_id) %>%
  summarise(
    avg_cf = mean(cessation_fatigue)) %>%
  ggplot(., aes(x = "", y = avg_cf)) +
  geom_boxplot(fill = "#3a86d4", alpha = 0.2) +
  geom_jitter(alpha = 0.5, color = "#3a86d4") +
  theme_minimal()

avg_cf <- sms %>%
  select(record_id, week, cessation_fatigue) %>%
  drop_na(cessation_fatigue) %>%
  group_by(record_id) %>%
  summarise(avg_cessation_fatigue = mean(cessation_fatigue))

# ///////////////////////////////////////////////////////////////////////////
#                             Merge data sets
# ///////////////////////////////////////////////////////////////////////////
t_to_relapse %<>%
  left_join(duration_act, by = "record_id")

t_to_relapse %<>%
  left_join(quit_attempts, by = "record_id")

t_to_relapse %<>%
  left_join(time_to_2nd_attempt, by = "record_id")

t_to_relapse %<>%
  left_join(n_hcs, by = "record_id")

t_to_relapse %<>%
  left_join(avg_cf, by = "record_id")

# ///////////////////////////////////////////////////////////////////////////
#                          Output to file
# ///////////////////////////////////////////////////////////////////////////
# Before outputing to .csv Convert dates to strings and save with na = ''
# to make compatible with SAS.
t_to_relapse %>%
  select(-record_id) %>%
  rename(censor = event, time_to_1st_relapse = time_to_relapse) %>%
  mutate_if(is.Date, as.character) %>%
  mutate_if(is.difftime, as.numeric) %>%
  write_csv(here("analyses_sas", "data", "first_relapse.csv"), na = "")