# /////////////////////////////////////////////////////////////////////////////
# Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine

# Step 1 of 3 - Create a lookup table for filtering results. Creates a files
# with combinations of DVs and PVs to use when fitting interaction models.

# /////////////////////////////////////////////////////////////////////////////

library(openxlsx)
library(tidyverse)

file_path <- "C:\\Users\\rodrica2\\OneDrive - The University of Colorado Denver\\Documents\\DFM\\projects\\hsq\\analyses_sas\\odds_of_smoking\\odds_of_smoking_by\\organized_results\\hsq_relapse_by_mo_results_2026-11-04.xlsx"

out_path <- "C:\\Users\\rodrica2\\OneDrive - The University of Colorado Denver\\Documents\\DFM\\projects\\hsq\\analyses_sas\\odds_of_smoking\\odds_of_smoking_w_int"


cat <- openxlsx::read.xlsx(xlsxFile = file_path, sheet = 2) %>% as_tibble()
cat <- cat %>%
  mutate(ProbChiSq = ifelse(ProbChiSq == "<.0001", ".0001", ProbChiSq)) %>%
  mutate(ProbChiSq = as.numeric(ProbChiSq)) %>%
  filter(ProbChiSq < 0.05) %>%
  select(Effect, DV) %>%
  rename(PV = Effect) %>%
  mutate(DV = str_replace(DV, "smoke1_SMS_by_12", "smoke1_SMS_by_12m")) %>%
  mutate(DV = str_c(DV, "o_jy")) %>%
  distinct() %>%
  write_csv(., str_c(out_path, "\\cat_sig_effects_lookup.csv"))


num <- openxlsx::read.xlsx(xlsxFile = file_path, sheet = 3) %>% as_tibble()
num <- num %>%
  mutate(ProbChiSq = ifelse(ProbChiSq == "<.0001", ".0001", ProbChiSq)) %>%
  mutate(ProbChiSq = as.numeric(ProbChiSq)) %>%
  filter(ProbChiSq < 0.05) %>%
  select(Effect, DV) %>%
  rename(PV = Effect) %>%
  mutate(DV = str_replace(DV, "smoke1_SMS_by_12", "smoke1_SMS_by_12m")) %>%
  mutate(DV = str_c(DV, "o_jy")) %>%
  mutate(PV = ifelse(PV == "rate_complete_MSurve", "rate_complete_MSurvey", PV)) %>%
  distinct() %>%
  write_csv(., str_c(out_path, "\\num_sig_effects_lookup.csv"))