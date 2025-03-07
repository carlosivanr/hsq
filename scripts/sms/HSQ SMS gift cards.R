# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Camille J. Hochheimer, Ph.D., Carlos Rodriguez Ph.D.
# Updated 02-20-2025

# Description:_________________________________________________________________
# Downloads data from HSQ SMS Survey REDCap project and uploads it to Helper
# Stay Quit Participant Managment project id 25710.

# Pulls data from report # 89001 titled "HSQ SMS Survey Count" in REDCap
# project id 26962. The REDCap report has 3 fields: 1) record_id; 2) hsq id;
# and 3) smssurvey_count. smssurvey_count is a calculated field that counts the
# number of sms surveys marked as complete from the field
# sms_survey_count_complete. The downloaded data is then modified to keep only
# the smssurvey_count and record_id columns. Finally, the modified data is
# uploaded to the Helper Stay Quit Participant Management project.

# This script is run automatically via Task Scheduler on HPC18 provisioned to
# Carlos Rodriguez by launching run_sms_gift_cards.bat
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# trouble with running script automatically:
# The following is the code from Janet's .qmd file that runs just fine on her 
# machine copied and pasted so it's the exact same code.

library(tidyverse)
#token for sms project
.sms_token <- Sys.getenv("HSQ_sms")
#pull report 	"SMS Survey Count"
url <- "https://redcap.ucdenver.edu/api/"
formData <- list(token = .sms_token, content = "report", format = "csv", 
        report_id = "89001", csvDelimiter = "", rawOrLabel = "raw", 
        rawOrLabelHeaders = "raw", exportCheckboxLabel = "false", 
        returnFormat = "csv")
response <- httr::POST(url, body = formData, encode = "form")
texts <- httr::content(response)

#create a dataset to upload into the HSQ Participant Management REDCap
gc <- texts %>%
  #need to get actual ID out of the hsqid it will match with the other RC
  mutate(id = as.numeric(str_sub(hsqid, - 4, - 1))) %>% 
  select(-redcap_event_name, -hsqid, -record_id) %>% 
  rename(record_id = id)
  
#upload to REDCap
REDCapR::redcap_write(gc,
                      redcap_uri = "https://redcap.ucdenver.edu/api/",
                      token = Sys.getenv("HSQ_api"))