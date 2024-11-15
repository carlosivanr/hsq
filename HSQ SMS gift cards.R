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
