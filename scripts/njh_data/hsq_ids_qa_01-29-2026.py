
# /////////////////////////////////////////////////////////////////////////////
# Carlos Rodriguez, Ph.D. CU Anschutz Dept. of Family Medicine

# 02/10/2026

# HSQ NJH Data QA:
# This script was written to compare the NJH data delivery from December 2025 
# to the delivery from January 2026, to determine which participant ids were 
# lost, and to investigate why. 

# Background: 
# On January 29th, 2026, NJH delivered a data extract to the HSQ study team.
# The purpose of the extract was to update a previous delivery from December
# 18, 2025. There were 18 rows in the january delivery that were not in the
# december delivery. According to NJH, these participants did not have an 
# intake date that was within 6 months of the randomization date when they
# enrolled in the study. 

# Results:
# Upon further examination of the 18 records, 9 of the 18 records were found in
# the december 2025 delivery AND had an intake date within 6 months of the 
# randomization date. So there could be some inconsistencies with how the data
# is being extracted. The remaining 9 did not have intake dates within a 
# reasonable amount of time. Some had a large gap between intake date associated
# the data and the randomization date. It begs the question on how these 
# participants were referred to the HSQ study.
# /////////////////////////////////////////////////////////////////////////////


#%% Load libraries ------------------------------------------------------------
import pandas as pd
pd.options.mode.copy_on_write = True

from pyprojroot import here
from datetime import datetime


#%% Load data -----------------------------------------------------------------
# Read as .xlsx since the intake dates in the extract were formatted in Excel, 
# which means data will be converted to numeric upon importing using csv.
data_20251218 = pd.read_excel(
  here("scripts/njh_data/data/prepped/njh_data_12-18-2025.xlsx"), 
  header = 0
)

data_20260129 = pd.read_excel(
  here("scripts/njh_data/data/prepped/njh_data_01-29-2026.xlsx"), 
  header = 0
)
# n.b. the Ohio data, which is in a separate file is not loaded, because it is
# not necessary to determine which ids were excluded from the January file when
# compared to the December file


#%% Determine which ids are missing and output to a csv file ------------------
# Pull the hsq ids that were not delivered on Jan 29 2026
missing_subset = data_20251218[~data_20251218["Hsqid"].isin(data_20260129["Hsqid"])]

# Output the missing hsq ids to a csv file
missing_subset["Hsqid"].to_csv(
  here("scripts/njh_data/data/data_qa/hsq_ids_not_in_njh_01-29-2026.csv"),
  index = False 
)


#%%  Inspect the randomization dates ------------------------------------------
# Inspect the randomization dates of those that were excluded from the January
# file.

# Randomization dates are available in the relapse data file in analyses_sas 
# directory. Load the relapse data file
relapse_data = pd.read_csv(here("analyses_sas/data/first_relapse.csv"))

# Filter those that are in missing data to get their randomization dates
relapse_data = (relapse_data[relapse_data["hsqid"]
  .isin(missing_subset["Hsqid"])][["hsqid", "randomization_dtd"]]
)

# Merge the randomization dates into the missing subset df. The two dfs use a
# different identifier that only differs by one is all in lower case.
missing_subset = pd.merge(
  missing_subset, 
  relapse_data, 
  how='left', 
  left_on = 'Hsqid', 
  right_on = 'hsqid'
)

# Create a separate subset with just the columns of interest
output = missing_subset[[
  "Participant Id", 
  "hsqid", 
  "randomization_dtd", 
  "PhoneIntakeDate"]]

# Write the output df a csv file
(output
  .to_csv(
    here("scripts/njh_data/data/data_qa/missing_hsq_ids_w_rand_dtd.csv"), 
    index = False)
)