library(tidyverse)

# Source get_redcap_report function
get_redcap_report <- function (token, report_id, labels = "raw")
{
    url <- "https://redcap.ucdenver.edu/api/"

   formData <- list(token = token, content = "report", format = "csv",
        report_id = report_id, csvDelimiter = "", rawOrLabel = labels,
        rawOrLabelHeaders = "raw", exportCheckboxLabel = "false",
        returnFormat = "csv")

    response <- httr::POST(url, body = formData, encode = "form")

    result <- httr::content(response)

    return(result)

}


.sms_token <- Sys.getenv("HSQ_sms")
texts <- get_redcap_report(token = .sms_token,
                                     report_id = "89001")

gc <- texts %>%
  select(-redcap_event_name, -hsqid)


#upload to REDCap
REDCapR::redcap_write(gc,
                      redcap_uri = "https://redcap.ucdenver.edu/api/",
                      token = Sys.getenv("HSQ_api"))
