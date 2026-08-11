################################################################################
# Carlos Rodriguez, PhD. CU Anschutz, Dept. of Fam. Medicine
# Helpers stay quit - prepare longitudinal data
#
# This script is designed to prepare the longitudinal data in the HSQ study.
# Longitudinal data refers to the participant management RedCap which collects
# the baseline, 3month, 6month, and 12 month, outcomes and psychological 
# instruments. Although baseline and 12month Personal network data are also
# collected in the same redcap project, this script does not prepare those 
# data. 

# Outputs a .csv file for analysis in SAS
################################################################################

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

data_lst <- list(
  (data %>%
  filter(event_name == "0mo") %>%
  filter(!is.na(days_since_rand))),

  (data %>%
  filter(event_name == "3mo") %>%
  filter(days_since_rand >= 90 + 14)),

  (data %>%
  filter(event_name == "6mo") %>%
  filter(days_since_rand >= 180 + 14)),

  (data %>%
  filter(event_name == "9mo") %>%
  filter(days_since_rand >= 270 + 14)),

  (data %>%
  filter(event_name == "12mo") %>%
  filter(days_since_rand >= 365 + 14)))

# Variable Processing /////////////////////////////////////////////////////////
# Gender
data_lst %<>%
  purrr::map(
    ~.x %>%
    mutate(Gender = recode(gender, 
      ",Man (including transman and transmasculine)" = "Man",
      "Woman (including transwoman and transfeminine)" = "Woman",
      "Prefer to self-describe (non-binary, gender queer) please specify below" = "Prefer to self-describe"))
  )

# Demographics: Race & Ethnicity, Age, Gender and Arm
# Convert the checked/unchecked response to 1/0 for tabulating
# *** Could possibly use group_by + fill with filter in a flat df

# Modifcations for tabulating
data_lst[[1]] %<>% 
  mutate(across(race___0:race____1, ~ ifelse(.x == "Checked", 1, 0))) %>%
  mutate(
    Hispanic = eth,
    "Black or African American" = race___0,
    "Asian" = race___1,
    "White/Caucasian" = race___2,
    "Native Hawaiian or other Pacific Islander" = race___3,
    "American Indian/Alaska Native" = race___4,
    "Other" = race____66,
    "Declined" = race____1,
    Arm = arm,
    Age = age)


# RETURN TO THIS FOR DATA PROCESSING SCRIPT
data_lst[[1]] %<>% 
  rowwise() %>%
  mutate(Multiple = sum(c_across(race___0:race____66))) %>%
  ungroup() %>%
  mutate(Multiple = ifelse(Multiple > 1, 1, 0)) %>%
  mutate(across(race___0:race____66, ~ if_else(Multiple == 1, 0, .x))) %>%
  mutate(
    race___0 = ifelse(race___0 == 1, "Black or African American", NA),
    race___1 = ifelse(race___1 == 1, "Asian", NA),
    race___2 = ifelse(race___2 == 1, "White/Caucasian", NA),
    race___3 = ifelse(race___3 == 1, "Native Hawaiian or other Pacific Islander", NA),
    race___4 = ifelse(race___4 == 1, "American Indian/Alaska Native", NA),
    race____66 = ifelse(race____66 == 1, "Other", NA),
    race____1 = ifelse(race____1 == 1, "Declined", NA),
    Multiple = ifelse(Multiple == 1, "Multiple", NA)) %>%
  mutate(Race = coalesce(race___0, race___1, race___2, race___3, race___4, race____66, race____1, Multiple))


# Score Psychological Instruments
# n.b. In cases where the responses are straightlined NA or "Prefer not to 
# answer", rowSums(..., na.rm = TRUE) will return a 0, instead of the expected 
# NA. To overcome this, multiply the rowSums(..., na.rm = TRUE) by NA raised to
# to a value of TRUE or FALSE. NA^FALSE = 1, whereas NA^TRUE = NA. The TRUE/FALSE 
# value of whether or not all questions are missing is given by 
# !rowSums(!is.na(select(., phq_1:phq_4))), where TRUE indicates all values are
# missing. Thus NA*TRUE will return NA which is used to multiply with the summed
# scores and if the score is a 0 resulting from all NAs, it will result in an NA.

## PHQ4
# - Collected at baseline, 3, 6, 9, and 12 months
# - 4 items phq_1:phq_4; 4-point Likert
# - ultra-brief screening scale for anxiety and depression
# - Composed of the first two items of the GAD7 and PHQ9
# - Total score ranges from 0 through 12
# - Convert -1s to NA, then sum scores
# - Check how many do not have all 4 items answered
# - Items 1 and 2 correspond to GAD7 (anxiety)
# - Items 3 and 4 correspond to PHQ (depression)
# - Total sum score of psychological distress:
#     - 0-2 - None
#     - 3-5 - Mild
#     - 6-8 - Moderate
#     - 9-12 - Severe

# Create anx, dep, and psy scores, which are sums of the individual sub scales 
# and the overall psychological distress scores
data_lst %<>%
  purrr::map( ~.x %>%
                mutate(across(phq_1:phq_4, ~recode(.,
                                                   "Not at all" = 0,
                                                   "Several days" = 1,
                                                   "More than half the days" = 2,
                                                   "Nearly every day" = 3,
                                                   "Prefer not to answer" = NULL))) %>%
                mutate(anx_score = rowSums(select(., phq_1:phq_2), na.rm = TRUE) * NA^!rowSums(!is.na(select(., phq_1:phq_2))),
                       dep_score = rowSums(select(., phq_3:phq_4), na.rm = TRUE) * NA^!rowSums(!is.na(select(., phq_3:phq_4))),
                       psy_score = rowSums(select(., phq_1:phq_4), na.rm = TRUE) * NA^!rowSums(!is.na(select(., phq_1:phq_4))),
                       anx_bin = ifelse(anx_score >= 3, 1, 0),
                       dep_bin = ifelse(dep_score >= 3, 1, 0),
                       psy_cat = cut(psy_score,
                                     breaks = c(-1,2,5,8,12),
                                     labels = c("None (0-2)", "Mild (3-5)", "Moderate (6-8)", "Severe (9-12)")),
                       PHQ4 = ifelse(is.na(psy_score), "Missing", "Not Missing"),
                       Anxiety = ifelse(is.na(anx_score), "Missing", "Not Missing"),
                       Depression = ifelse(is.na(dep_score), "Missing", "Not Missing")
                       )
  )

## Abstinence-related motivational engagement (ARME) scale (short form): 
# - Collected at baseline, 3, 6, 9, and 12 months
# - 5 items arme_1:arme_5, 7-point Likert
# - Assesses motivation to remain abstinent after smoking cessation attempt
# - Convert categorical text to numerical
# - Convert -1s to NA, then sum scores
# - Count how many have a complete response i.e. no missing responses
# - Check for straightliners???

data_lst %<>%
  purrr::map( ~.x %>%
                mutate(across(arme_1:arme_5, ~recode(.,
                                       "Completely disagree" = 1,
                                       "2" = 2,
                                       "3" = 3,
                                       "Neither agree nor disagree" = 4,
                                       "5" = 5,
                                       "6" = 6,
                                       "Completely agree" = 7,
                                       "Prefer not to answer" = NULL))) %>%
                mutate(arme_score = rowSums(select(., arme_1:arme_5), na.rm = TRUE) * NA^!rowSums(!is.na(select(., arme_1:arme_5))),
                       ARME = ifelse(is.na(arme_score), "Missing", "Not Missing"))
  )

## Confidence Self-Efficacy
# - Collected at baseline, 3, 6, 9, and 12 months
# - 5 items 1:5 are about confidence
# - scored and 4 point Likert scale
# - From Campbell et al., 2007
data_lst %<>%
  purrr::map(
    ~.x %>%
  mutate(across(se1:se5, ~ case_match(.,
    "Very Confident" ~ 3,
    "Somewhat confident" ~ 2,
    "Not very confident" ~ 1,
    "Not at all confident" ~ 0,
    .default = NA))) %>%
  mutate(
    se_conf_score = ((rowSums(select(., se1:se5), na.rm = TRUE) / 15) * 100) * NA^!rowSums(!is.na(select(., se1:se5))),
    SE_c = ifelse(is.na(se_conf_score), "Missing", "Not Missing"))
  )  


## Skills Self-Efficacy
# - Collected at baseline, 3, 6, 9, and 12 months
# - 7 items 6:12 are about self efficacy
# - scored on a 7 point Likert scale ranges from -3 to 3
# - From Dijkstra & DeVries 2007
data_lst %<>%
  purrr::map(
    ~.x %>%
    mutate(across(se6:se12, ~ case_match(.,
      "Not at all sure I am able to" ~ -3,
      "Usually not sure I am able to" ~ -2,
      "Somewhat not sure I am able to" ~ -1,
      "Neutral" ~ 0,
      "Somewhat sure I am able to" ~ 1,
      "Usually sure I am able to" ~ 2,
      "Very sure I am able to" ~ 3,
      .default = NA))) %>%
    mutate(
      se_skill_score = ((rowMeans(select(., se6:se12), na.rm = TRUE) * NA^!rowSums(!is.na(select(., se6:se12))))),
      SE_s = ifelse(is.na(se_skill_score), "Missing", "Not Missing"))
  )


## Proactive coping scale (part of the proactive coping inventory)
# - Collected at baseline, 3, 6, 9, and 12 months
# - 14 items, pcs_01:pcs_14, 
# - Assesses future oriented coping (effectively managing stressful events)
# - Reverse score items 2, 9, 14
# - Convert categorical text to numerical
# - Convert -1s to NA, then sum scores

# Convert categorical responses to numeric, reverse code, and sum the responses
# rowSums of all NAs results in 0, thus we need to multiply sums 
# by a vector that describes whether or not all values were NA, 
# so that summed 0 scores resulting from all NAs are converted
# NAs. Equivalent to set value to NA if number of NAs equals 14, 
# else leave as original value 
# !rowSums(!is.na(select(., pcs_01:pcs_14))) gives a logical, true/false if all the values in the row
# are NA.
data_lst %<>%
  purrr::map( 
    ~.x %>%
    mutate(across(pcs_01:pcs_14, ~ recode(.,
      "Not at all true" = 1,
      "Barely true" = 2,
      "Somewhat true" = 3,
      "Completely true" = 4,
      "Prefer not to answer" = NULL))) %>%
    # Reverse score items 2, 9, and 14
    mutate(across(c(pcs_02, pcs_09, pcs_14), ~ . * -1 + 5)) %>%
    mutate(
      pcs_score = rowSums(select(., pcs_01:pcs_14), na.rm = TRUE) * NA^!rowSums(!is.na(select(., pcs_01:pcs_14))),
      PCS = ifelse(is.na(pcs_score), "Missing", "Not Missing"))
  )


# COGDIS
# - Collected at baseline, 3, 6, 9, and 12 months
# This is named CD/COGDIS in RedCap, but it's about residual attraction to 
# smoking, smoking identity, and vulnerability to smoking relapse. Perhaps it
# was meant to look at the congruence between smoking identity and attraction
# and/or vulnerability as a measure of cognitive dissonance.

# Set any value of "Prefer not to answer" between cd1 and cd6 to NA. Then, 
# dichotomize answers by setting Yes a little/Yes a lot to 1, or Definitely/Probably
# to 1. Then create the following 3 new variables:

# 1. cd_attraction - binary indicator of residual attraction to smoking, set to 1
# if the participant responded Yes a little or Yes a lot to one of cd1:cd3, else 
# set to 0

# 2. cd_smoker - binary indicator of self reported label, set to 0 if the 
# participant responded definitely non smoker to cd4, else set to 1

# 3. cd_vulnerability - binary indicator of vulnerability to smoking relapse, set 
# to 1 if participant responded Definitely or Probably to cd5:cd6, else set to 0

data_lst %<>%
  purrr::map( 
    ~.x %>%
    mutate(across(cd1:cd6, ~ifelse(. == "Prefer not to answer", NA, .))) %>%
    mutate(
      across(cd1:cd2, ~ifelse(. == "Yes a little" | . == "Yes a lot", 1, 0)),
      across(c(cd3, cd5, cd6), ~ifelse(. == "Definitely" | . == "Probably", 1, 0))) %>%
    mutate(
      cd_attraction = ifelse(cd1 == 1 | cd2 == 1 | cd3 == 1, 1, 0),
      cd_smoker = ifelse(cd4 == "Definitely a non-smoker", 0, 1),
      cd_vulnerable = ifelse(cd5 == 1 | cd6 == 1, 1, 0)) %>%
    mutate(
      CD_att = cd_attraction,
      CD_idn = cd_smoker,
      CD_vul = cd_vulnerable) %>%
    mutate(across(CD_att:CD_vul, ~ ifelse(is.na(.), "Missing", "Not Missing")))
  )


# Treatment Self Regulation Questionnaire (TSRQ)
# - Collected at baseline, 3, 6, 9, and 12 months
# - 15 items, tsrq1:tsrq15, 7-point Likert
# - Contains 4 sub scales
#     - items within each subscale are averaged
# - Autonomous motivation (factor 1)
#   - Items 13, 3, 1, 8, 6, 11
# - Introjected regulation (factor 2)
#   - Items 2, 7,
# - External regulation (factor 3)
#   - Items 9, 4, 14, 12
# - Amotivation (factor 4)
#   - Items 5, 15, 10

data_lst %<>%
  purrr::map( 
    ~.x %>%
    mutate(across(tsrq1:tsrq15, ~ recode(.,
      "1 - Not at all true" = 1,
      "2" = 2,
      "3" = 3,
      "4 - Somewhat true" = 4,
      "5" = 5,
      "6" = 6,
      "7 - Very true" = 7,
      "Prefer not to answer" = NULL))) %>%
    rowwise() %>%
    mutate(
      tsrq_auto_score = mean(c(tsrq13, tsrq3, tsrq1, tsrq8, tsrq6, tsrq11), na.rm = TRUE),
      tsrq_intr_score = mean(c(tsrq2, tsrq7), na.rm = TRUE),
      tsrq_extr_score = mean(c(tsrq9, tsrq4, tsrq14, tsrq12), na.rm = TRUE),
      tsrq_amot_score = mean(c(tsrq5, tsrq15, tsrq10), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      TSRQ_auto = tsrq_auto_score,
      TSRQ_intr = tsrq_intr_score,
      TSRQ_extr = tsrq_extr_score,
      TSRQ_amot = tsrq_amot_score) %>%
    mutate(across(TSRQ_auto:TSRQ_amot, ~ ifelse(is.na(.), "Missing", "Not Missing")))
  )


# Output to SAS for analysis //////////////////////////////////////////////////
# Flatten the list
flat_lst <- bind_rows(
  data_lst[[1]],
  data_lst[[2]],
  data_lst[[3]],
  data_lst[[4]],
  data_lst[[5]]
)

# Select only the variables of interest

flat_lst %>%
  select(-record_id, -ed) %>%
  select(
    hsqid, event_name, Arm, quitline, Gender, Age, Race,
    anx_score, dep_score, psy_score, anx_bin, dep_bin, psy_cat,
    arme_score, se_conf_score, se_skill_score, pcs_score,
    cd_attraction:cd_vulnerable,
    tsrq_auto_score:tsrq_amot_score,
    stay_quit_method___1:stay_quit_method_other,
    help1:ne6
  ) %>%
  arrange(hsqid, event_name) %>%
  mutate_if(is.Date, as.character) %>%
  mutate_if(is.difftime, as.numeric) %>%
  group_by(hsqid) %>%
  fill(Arm, Age, Race, .direction = "downup") %>%
  ungroup() %>%
  write_csv(here("analyses_sas", "data", "hsq_data.csv"), na = "")