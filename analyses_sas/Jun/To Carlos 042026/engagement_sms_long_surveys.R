# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# The following script was written to conduct the correlation between the 
# ARME Score at the baseline time point with the completion rate of the SMS and
# the longitudinal surveys at the time of the data lock which was Mar-26-2026.

# The results indicate that the correlations are very small between these two
# variables and the ARME and are not significant.

# Relies on Jun's sas data set, originally placed in Egnyte, but copied over to
# OneDrive for local availability.
# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
library(tidyverse)
library(haven)
library(gtsummary)

data <- read_sas("C:\\Users\\rodrica2\\OneDrive - The University of Colorado Denver\\Documents\\DFM\\projects\\hsq\\analyses_sas\\Jun\\To Carlos 042026\\bldat_0326.sas7bdat")

data <- data %>%
  mutate(across(where(is.character), zap_empty))

# Create prop complete variables
data <- data %>%
  mutate(
    prop_complete_SMS = total_complete_SMS/total_due_SMS,
    prop_complete_M = total_complete_MSurvey / total_due_MSurvey)

cor.test(
  data$prop_complete_SMS,
  data$arme_score,
  method = c("pearson"))

# Stronger correlation between longitudinal surveys and arme compared to sms surveys
cor.test(
  data$prop_complete_M,
  data$arme_score,
  method = c("pearson"))