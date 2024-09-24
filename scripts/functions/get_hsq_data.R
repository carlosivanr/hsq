# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. Dept. of Family Medicine. CU Anschutz

# get_hsq_data()

# This function was designed to download the most recently available data from
# the Helpers Stay Quit research project in RedCap. More specifically, this
# function downloads from individual reports built in the HSQ Participant
# Management RedCap project. Each report corresponds to one of the main
# 0 month (baseline), 3 month, 6 month, and 12 month surveys. Data corresponding
# to each time point is downloaded separately to facilitate the creationg of one
# long format data set. Data from each time point can be prepared and modified
# individually so that the columns in each data set are consistent. For example,
# in the 3 month survey, the column names for an instrument will have a "_3"
# appended to the column names. For the same instrument, in the 6 month survey,
# the column names will have a "_6" appended. Each time point is downloaded
# separately to modify these column names which will facilitate staking the
# datasets. In addition the social network columns from the baseline (0 month)
# and 12 month surveys are removed to facilitate data stacking. Once the data
# sets are stacked, then the same processing function like changing character
# to numeric can be perfromed on one data set as opposed to multiple times on
# multiple data sets.

# Usage:
# data <- get_hsq_data()

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_hsq_data <- function(){
  # Set token for the hsq red cap project
  .token_hsq <- Sys.getenv("HSQ_api")

  # Set token for the sms red cap project
  .token_sms <- Sys.getenv("HSQ_sms")

  # Set Redcap URL
  url <- "https://redcap.ucdenver.edu/api/"

  # Set formData shell
  # n.b. only token and report_id need modification in subsequent lines of code
  # "token" and report_id set to blank
  formData <- list("token"='',
                   content='report',
                   format='csv',
                   report_id='',
                   csvDelimiter='',
                   rawOrLabel='label',
                   rawOrLabelHeaders='raw',
                   exportCheckboxLabel='false',
                   returnFormat='json')


  # Pull from the hsq participant management redcap project

  # Set the token to the appropriate redcap project
  formData$token <- .token_hsq

  ############################## Survey_0mo ####################################
  # Set the report id to the 0 month report
  formData$report_id <- "112310" # baseline

  # Create response object using formData
  response <- httr::POST(url, body = formData, encode = "form")

  # Create data frame from response object
  survey_0mo <- httr::content(response)

  # Cull columns
  survey_0mo %<>%
    select(record_id:stay_quit_method_other,
           randomization_dtd) %>%
    select(-starts_with("timestamp"))

  # Create a redcap_event_name column
  survey_0mo %<>%
    mutate(event_name = "0mo")


  ############################## Survey_3mo ####################################
  # Set the report id to the 3 month report
  formData$report_id <- "112311" # baseline

  # Create response object using formData
  response <- httr::POST(url, body = formData, encode = "form")

  # Create data frame from response object
  survey_3mo <- httr::content(response)

  # Set the names of the columns where the 3mo suffix has been removed
  names_to_replace <- sub("_3m", "", names(survey_3mo))

  # Replace the names of the columns to match baseline
  colnames(survey_3mo) <- names_to_replace

  # Set event name
  survey_3mo %<>%
    mutate(event_name = "3mo") %>%
    select(-survey_3_month_complete) %>%
    mutate(help1 = as.numeric(help1))


  ############################## Survey_6mo ####################################
  # Set the report id to the 6 month report
  formData$report_id <- "112312" # baseline

  # Create response object using formData
  response <- httr::POST(url, body = formData, encode = "form")

  # Create data frame from response object
  survey_6mo <- httr::content(response)

  # Set the names of the columns where the 3mo suffix has been removed
  names_to_replace <- sub("_6m", "", names(survey_6mo))

  # Replace the names of the columns to match baseline
  colnames(survey_6mo) <- names_to_replace

  # Set event name
  survey_6mo %<>%
    mutate(event_name = "6mo") %>%
    select(-survey_6_month_complete) %>%
    rename(stay_quit_method_other = stayquit_other) %>%
    mutate(help1 = as.numeric(help1))

  # names in 6mo data no in 3mo data
  # names(survey_6mo)[!names(survey_6mo) %in% names(survey_3mo)]

  # contains ne* questions
  survey_6mo$zip_code_ne <- as.numeric(survey_6mo$zip_code_ne)


  ############################## Survey_9mo ####################################
  # Set the report id to the 9 month report
  formData$report_id <- "112313" # baseline

  # Create response object using formData
  response <- httr::POST(url, body = formData, encode = "form")

  # Create data frame from response object
  survey_9mo <- httr::content(response)

  # Set the names of the columns where the 3mo suffix has been removed
  names_to_replace <- sub("_9m", "", names(survey_9mo))

  # Replace the names of the columns to match baseline
  colnames(survey_9mo) <- names_to_replace

  # Set event name
  survey_9mo %<>%
    mutate(event_name = "9mo") %>%
    select(-survey_9_month_complete)

  # names in 9mo data not in 6mo data
  # names(survey_6mo)[!names(survey_6mo) %in% names(survey_9mo)]
  # names(survey_9mo)[!names(survey_9mo) %in% names(survey_6mo)]


  ############################## Survey_12mo ###################################
  # Set the report id to the 12 month report
  formData$report_id <- "112314" # baseline

  # Create response object using formData
  response <- httr::POST(url, body = formData, encode = "form")

  # Create data frame from response object
  survey_12mo <- httr::content(response)

  # Set event name
  survey_12mo %<>%
    select(record_id:quit_again_method_other_12m,
           randomization_dtd) %>%
    mutate(event_name = "12mo") %>%
    select(-timestamp_1_12m)

  # Set the names of the columns where the 3mo suffix has been removed
  names_to_replace <- sub("_12m", "", names(survey_12mo))

  # Replace the names of the columns to match baseline
  colnames(survey_12mo) <- names_to_replace
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ####### Stack and process data ------------------------------------------------
  # Some columns may get imported as character instead of numeric, place
  # all data frames into a survey to modify and obtain a consistent format
  survey_6mo$fu_smoke6 <- as.numeric(survey_6mo$fu_smoke6)
  survey_9mo$fu_smoke2 <- as.numeric(survey_9mo$fu_smoke2)

  # Bind rows to keep a data frame of the full data
  data <- bind_rows(survey_0mo, survey_3mo, survey_6mo, survey_9mo, survey_12mo)

  # Create a randomization date variable to be used for filtering participants
  # the appropriate length of time they have been in the study
  data %<>%
    mutate(days_since_rand = Sys.Date() - as.Date(randomization_dtd))

  # Process variables
  data %<>%
    mutate(arm = ifelse(arm == "1-  HSQ training - beginning of participation", "Intervention", "Control"))


  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ####### Download and merge HSQ ids and quitlines --------------------------
  # Set the report id to the survey_hsqids report in the redcap project
  formData$report_id <- "123557"

  # Create response object using formData
  response <- httr::POST(url, body = formData, encode = "form")

  # Create data frame from response object
  hsq_ids <- httr::content(response)

  hsq_ids %<>%
    drop_na(hsqid) %>%
    filter(record_id %in% data$record_id)


  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Merge data sets only if the join will not introduce records
  if(data %>% nrow() == left_join(data, hsq_ids, by = "record_id") %>% nrow()) {
    data %<>%
      left_join(., hsq_ids, by = "record_id", relationship = "many-to-many")
  } else {
      stop("Merging data with hsq_ids results in the introduction of aberrant rows.")
    }


  ####### Return output --------------------------------------------------------
  return(data)

}
