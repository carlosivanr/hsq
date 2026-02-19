
# /////////////////////////////////////////////////////////////////////////////
# Carlos Rodriguez, Ph.D. CU Anschutz Dept. of Family Medicine

# 02/17/2026

# HSQ NJH Data QA:
# This script was written to compare the NJH data delivery from January 2026 
# to the delivery from February 2026, to determine which participant ids were 
# lost, and to investigate why. 

# Background & Results:
# December 2025 delivery had a total of 857 rows of data. However, there were
# two rows with the same Id, 10295, where there was ony one row per participant
# expected. So the unique number of ids delivered in December was actually 856.
# The OH data contains 83 rows. So the total was 939 total ids.

# The number we've been working off of is 940. These were randomized, 
# consented, participants. 

# Of the 940 set of participants, 10252 was missing from the all NJH deliveries
# as far back as October 2024. They seemed to have submitted a baseline survey
# but do not have any other timepoints for the main dataset. They also do not
# have any of the sms text message survey data.

# The NJH data delivered on February 13, 2026 contained 856 ids, plus the 83 
# from OH, equals 939. The same id, 10252, was not in the data.

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

data_20260213 = pd.read_csv(
  here("scripts/njh_data/data/prepped/njh_data_02-13-2026.csv"), 
  header = 0
)

# Load the OH data
oh = pd.read_csv(
  here("scripts/njh_data/data/prepped/njh_data_OH_03-21-2025.csv"), 
  header = 0)


#%% Determine which ids are missing and output to a csv file ------------------
# Pull the hsq ids that were not delivered on Jan 29 2026
data_20251218[~data_20251218["Hsqid"].isin(data_20260213["Hsqid"])]

# This returns all false, it means none of the ids in december are found in 
# february
test_vector = ~data_20251218["Hsqid"].isin(data_20260213["Hsqid"])

# This returns all true. 
test_vector = data_20251218["Hsqid"].isin(data_20260213["Hsqid"])

# Count number of rows after grouping
counts = data_20260213.groupby("Hsqid").size()

# Count number of rows after grouping
# !!! This reveals that the December 18 2025 data extract had two rows for HSQ 
# id 10295. So theres another missing id somewhere.
counts = data_20251218.groupby("Hsqid").size()

#%% Load a master file with all of the HSQ ids, to determine which one is missing.

redcap_data = pd.read_csv(
  here("analyses_sas/data/hsq_data.csv"), 
  header = 0
)

redcap_hsq_ids = pd.DataFrame( {"Hsqid": redcap_data["hsqid"].unique()})

# del(redcap_data)

#%%
intake_dates = oh[["Hsqid", "PhoneIntakeDate"]]

rand_dates = redcap_data[["hsqid", "randomization_dtd"]].drop_duplicates()

# Merge the two files
merged_oh = pd.merge(intake_dates, rand_dates, left_on= "Hsqid", right_on = "hsqid")

# Convert to datetime
merged_oh['PhoneIntakeDate'] = pd.to_datetime(merged_oh['PhoneIntakeDate'])
merged_oh['randomization_dtd'] = pd.to_datetime(merged_oh['randomization_dtd'])

merged_oh["diff"] = (merged_oh['PhoneIntakeDate'] - merged_oh['randomization_dtd']).dt.days


#%%
redcap_hsq_ids[~redcap_hsq_ids["Hsqid"].isin(oh["Hsqid"])]

redcap_hsq_ids[~redcap_hsq_ids["Hsqid"]
  .isin(
    data_20260213["Hsqid"]
    )]

set(redcap_hsq_ids["Hsqid"]).intersection(set(oh["Hsqid"]))


redcap_hsq_ids["Hsqid"].isin(oh["Hsqid"]).value_counts()

filtered = redcap_hsq_ids[~redcap_hsq_ids["Hsqid"].isin(oh["Hsqid"])]

filtered = filtered[~filtered["Hsqid"].isin(data_20260213["Hsqid"])]
