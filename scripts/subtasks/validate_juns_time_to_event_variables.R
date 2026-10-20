################################################################################
# Carlos Rodriguez, PhD. CU Anschutz, Dept. of Fam. Medicine
# Helpers stay quit - validate Jun's time to event and logistic model variables
#
################################################################################

pacman::p_load(here,
               tidyverse,
               magrittr,
               gtsummary,
               Hmisc, 
               gt,
               haven,
               install = FALSE)


# Create a file for planning out which variables need to be validated.

juns_sas_file <- "Z:\\Shared\\DFM\\HSQ_Shared\\Analysis\\Stat Working\\Datasets\\outcome_092926.sas7bdat"


juns_sas_file2 <- "Z:\\Shared\\DFM\\HSQ_Shared\\Analysis\\Stat Working\\Datasets\\outcome_analysis100826.sas7bdat"

View(
  data.frame(
  variable = read_sas(juns_sas_file) %>%
  names()
)
)

# ////////////////////////////////////////////////////////////////////////////
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
raw_sms <- get_redcap_report(token = Sys.getenv("HSQ_sms"), report_id = "117325")

sms <- raw_sms

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

# Count the number of missing values between smoke1 and help1
sms %<>%
  mutate(
    n_missing = rowSums(is.na(across(smoke1:help1))))

# If the number of missing is less than 7, then the participant answered at
# least 1 question
sms %<>%
  mutate(all_missing = ifelse(n_missing == 7, 1, 0))

# Turn the NAs to 0. Since an ITT analysis requires all participants
# they need to be set to No, so the comparison will be Yes vs not Yes to the
# smoke1 question.
sms %<>%
  mutate(smoke1 = ifelse(is.na(smoke1), 0, smoke1))

create_t_to_relapse <- function(variable){

 # Capture the time to relapse for those that relapsed in weeks
  event1 <- sms %>%
    filter(!!sym(variable) == 1) %>%
    group_by(hsqid) %>%
    slice_head() %>%
    ungroup() %>%
    mutate(event1 = 1) %>%
    select(hsqid, event1, week) %>%
    rename(surv1_week = week)

  # For those that did not relapse, need the last observation
  # Capture the last observation of those who did not relapse and provided at
  # least one response to one question at one of the sms surveys
  event0_all_not_missing <- sms %>%
    filter(!hsqid %in% event1$hsqid) %>%
    filter(all_missing == 0) %>%
    group_by(hsqid) %>%
    arrange(hsqid, week) %>%
    slice_tail() %>%
    ungroup() %>%
    mutate(event1 = 0) %>%
    select(hsqid, event1, week)

  # For those that did not relapse and did not provide any responses at all, then
  # they get an NA
  event0_all_missing <- sms %>%
    filter(!hsqid %in% c(event1$hsqid, event0_all_not_missing$hsqid)) %>%
    group_by(hsqid) %>%
    arrange(hsqid, week) %>%
    slice_tail() %>%
    ungroup() %>%
    mutate(event1 = 0, week = NA) %>%
    select(hsqid, event1, week)

  # For those that did not relapse there are two routes.
  # Those that did not relapse and we have a time stamp and those that did not
  # relapse but there is a time stamp
  # *** Commented to align with Jun's approach
  event0_w_time_stmp <- sms %>%
    filter(!hsqid %in% event1$hsqid) %>%
    select(hsqid, !!sym(variable), week, sms_survey_timestamp) %>%
    drop_na() %>%
    group_by(hsqid) %>%
    arrange(week) %>%
    slice_tail() %>%
    ungroup() %>%
    mutate(event1 = 0) %>%
    select(hsqid, event1, week) %>%
    rename(surv1_week = week)

  # People for which did not have evidence of relapse, but no time stamps
  # *** Commented to align with Jun's approach
  event0_w_out_time_stmp <- sms %>%
    filter(!hsqid %in% c(event1$hsqid, event0_w_time_stmp$hsqid)) %>%
    select(hsqid, !!sym(variable), week) %>%
    group_by(hsqid) %>%
    slice_tail() %>%
    ungroup() %>%
    mutate(surv1_week = NA, event1 = 0) %>%
    select(hsqid, event1, surv1_week, -!!sym(variable), -week)

  # Older version
  # temp <- bind_rows(
  #   event1,
  #   event0_w_time_stmp,
  #   event0_w_out_time_stmp) %>%
  #   arrange(hsqid)

  
  # Newer version
  temp <- bind_rows(
    event1,
    event0_all_missing,
    event0_all_not_missing) %>%
    arrange(hsqid)

  return(temp)

}

# ///////////////////////////////////////////////////////////////////////////
#                         Time to first relapse
# ///////////////////////////////////////////////////////////////////////////
part1 <- create_t_to_relapse("smoke1")

# ///////////////////////////////////////////////////////////////////////////
#                    Time to two consecutive relapses
# ///////////////////////////////////////////////////////////////////////////
sms %<>%
  group_by(record_id) %>%
  mutate(
    consecutive_relapse = ifelse(smoke1 == 1 & lag(smoke1) == 1, 1, 0)
  ) %>%
  ungroup() %>%
  mutate(consecutive_relapse = 
    ifelse(is.na(consecutive_relapse), 0, consecutive_relapse))

# Algorithm not set up to take a dynamic name input, but can be renamed here 
# for now.
create_t_to_relapse2 <- function(variable){

 # Capture the time to relapse for those that relapsed in weeks
  event1 <- sms %>%
    filter(!!sym(variable) == 1) %>%
    group_by(hsqid) %>%
    slice_head() %>%
    ungroup() %>%
    mutate(event1 = 1) %>%
    select(hsqid, event1, week) %>%
    rename(surv1_week = week)

  # For those that did not relapse, need the last observation
  # Capture the last observation of those who did not relapse and provided at
  # least one response to one question at one of the sms surveys
  event0_all_not_missing <- sms %>%
    filter(!hsqid %in% event1$hsqid) %>%
    filter(all_missing == 0) %>%
    group_by(hsqid) %>%
    arrange(hsqid, week) %>%
    slice_tail() %>%
    ungroup() %>%
    mutate(event1 = 0) %>%
    select(hsqid, event1, week)

  # For those that did not relapse and did not provide any responses at all, then
  # they get an NA
  event0_all_missing <- sms %>%
    filter(!hsqid %in% c(event1$hsqid, event0_all_not_missing$hsqid)) %>%
    group_by(hsqid) %>%
    arrange(hsqid, week) %>%
    slice_tail() %>%
    ungroup() %>%
    mutate(event1 = 0, week = NA) %>%
    select(hsqid, event1, week)

  # Newer version
  temp <- bind_rows(
    event1,
    event0_all_missing,
    event0_all_not_missing) %>%
    arrange(hsqid) %>%
    mutate(surv1_week = coalesce(surv1_week, week)) %>%
    select(-week)

  return(temp)

}

part2 <- create_t_to_relapse2("consecutive_relapse") %>%
  rename(event11 = event1, surv11_week = surv1_week)


test <- 
  part2 %>%
  left_join(
    jun_1 %>% select(hsqid, surv11_week_jy),
    by = "hsqid"
  ) %>%
  mutate(matches = ifelse(surv11_week == surv11_week_jy, 1, 0))

test %>%
  filter(hsqid %in% c( 10554, 10572, 60826))


# ///////////////////////////////////////////////////////////////////////////
#                    Relapse at 3, 6, 9, 12 months
#                        12, 24, 36, 48 weeks
# ///////////////////////////////////////////////////////////////////////////
weeks <- c(12, 24, 36, 48)

# Function to extract smoke1 from sms or smoke5 from quarterly survey
extract_smoke_sms_at <- function(x) {
    if (x == 12) {
      mo = 3
    } else if (x == 24) {
      mo = 6
    } else if (x == 36) {
      mo = 9
    } else if (x == 48) {
      mo = 12
    }

  name_out <- str_c("smoke1_SMS_at_", mo, "mo")

  temp <- sms %>%
    filter(week == x) %>%
    select(hsqid, smoke1) %>%
    rename(!!rlang::sym(name_out) := smoke1)

  return(temp)

  }

sms_smoke1_vars_at <- weeks %>%
  map(~ extract_smoke_sms_at(.x))


# ///////////////////////////////////////////////////////////////////////////
#                    Relapse by 3, 6, 9, 12 months
#                        12, 24, 36, 48 weeks
# ///////////////////////////////////////////////////////////////////////////
weeks <- c(12, 24, 36, 48)

extract_smoke_sms_by <- function(x, var) {
  if (x == 12) {
    mo = 3
  } else if (x == 24) {
    mo = 6
  } else if (x == 36) {
    mo = 9
  } else if (x == 48) {
    mo = 12
  }

  if (var == "smoke1") {
    name_out <- str_c("smoke1_SMS_by_", mo, "mo")
  } else {
    name_out <- str_c("smoke11_SMS_by_", mo, "mo")
  }

  temp <- sms %>%
    filter(week == x) %>%
    select(hsqid, !!rlang::sym(var)) %>%
    group_by(hsqid) %>%
    summarise(!!rlang::sym(var) := sum(!!rlang::sym(var))) %>%
    mutate(!!rlang::sym(var) := ifelse(!!rlang::sym(var) > 0, 1, 0)) %>%
    rename(!!rlang::sym(name_out) := !!rlang::sym(var))

  return(temp)
}

sms_smoke1_vars_by <- weeks %>%
  map(~ extract_smoke_sms_by(.x, "smoke1"))

sms_smoke11_vars_by <- weeks %>%
  map(~ extract_smoke_sms_by(.x, "consecutive_relapse"))


# ///////////////////////////////////////////////////////////////////////////
#      7-day Relapse by 3, 6, 9, 12 months using quarterly survey data
#                        12, 24, 36, 48 weeks
# ///////////////////////////////////////////////////////////////////////////
# Loads the function that updates data from the hsq participant management project
# source(here("Helpers-Stay-Quit", "03 Code/functions/get_hsq_data.R"))
source(here("scripts/functions/get_hsq_data.R" ))

months <- c("3mo", "6mo", "9mo", "12mo")

data <- get_hsq_data() %>%
  filter(event_name %in% months) %>%
  select(hsqid, event_name, fu_smoke5) %>%
  mutate(fu_smoke5 = ifelse(is.na(fu_smoke5), 0, ifelse(fu_smoke5 == "Yes", 1, 0)))

extract_smoke5 <- function(x) {

  name_out <- str_c("smoke5_survey_at_", x)
  
  temp <- data %>%
    filter(event_name == x) %>%
    select(hsqid, fu_smoke5) %>%
    rename(!!rlang::sym(name_out) := fu_smoke5)

  return(temp)
}

smoke5_vars <- months %>%
  map(~ extract_smoke5(.x))


# ///////////////////////////////////////////////////////////////////////////
#      Stitch data sets together and write to .csv
# ///////////////////////////////////////////////////////////////////////////

list(
  part1,
  part2,
  sms_smoke1_vars_at %>% reduce(left_join, by = "hsqid"),
  sms_smoke1_vars_by %>% reduce(left_join, by = "hsqid"),
  sms_smoke11_vars_by %>% reduce(left_join, by = "hsqid"),
  smoke5_vars %>% reduce(left_join, by = "hsqid")) %>%
  reduce(left_join, by = "hsqid") %>%
  write_csv(
    ., 
    "C:\\Users\\rodrica2\\OneDrive - The University of Colorado Denver\\Documents\\DFM\\projects\\hsq\\scripts\\subtasks\\validate_data.csv", 
    na = "")