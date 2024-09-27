# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. CU Anschutz Department of Family Medicine
# 03/08/2024

# HSQ Qualitative Dyad Report
# The purpose of this script is to produce an xlsx file containing information
# to contact participants for qualitative interviews as part of the HSQ study.
# HSQ_api is set through environment variables
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load the project -------------------------------------------------------------
# deprecated since the script will run from a windows batch file which will
# contain the command to open the project.
# renv::load("C:/Users/rodrica2/OneDrive - The University of Colorado Denver/Documents/DFM/projects/hsq/HSQ-Reports")

# Load libraries ---------------------------------------------------------------
# library(here)
pacman::p_load(tidyverse,
               magrittr,
               writexl, install = FALSE)

# Print the directory for here for testing -------------------------------------
# vestigial does not output to batch file log
# print(here())


# Pull RedCap Data -------------------------------------------------------------
# 1) Set formData shell
# formData will be used by httr::POST() to genereate the RedCap report response
# which in turn will be used to extract a data frame of the report

# Set token for the hsq red cap project
.token_hsq <- Sys.getenv("HSQ_api")

# Set Redcap URL
url <- "https://redcap.ucdenver.edu/api/"

# n.b. only token and report_id need modification in subsequent lines of code
# "token" and report_id set to blank
formData <- list("token"=.token_hsq,
                 content='report',
                 format='csv',
                 report_id='109599',
                 csvDelimiter='',
                 rawOrLabel='label',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 returnFormat='json')

# Pull RedCap data using formData from the hsq participant management redcap project
# Create response object using formData
response <- httr::POST(url, body = formData, encode = "form")

# Create data frame from response object
hsq_eligible <- httr::content(response)


# Clean and prep RedCap data ---------------------------------------------------
# Create months of participation
hsq_eligible %<>%
  mutate(n_days_partic = as.numeric(difftime(Sys.Date(), randomization_dtd, units = "days"))) %>%
  mutate(n_weeks_partic = round(n_days_partic/7)) %>%
  select(-n_days_partic)

# Convert gender to a binary value and convert arm to 2 or 1, rename columns,
# and remove unneccessary columns
data <-
  hsq_eligible %>%
  mutate(gender_bin_m = ifelse(gender == ",Man (including transman and transmasculine)", 1,
                             ifelse(gender == "Woman (including transwoman and transfeminine)", 0, NA))) %>%
  mutate(arm = ifelse(arm == "2 - HSQ training - end of participation", "2", "1")) %>%
  rename(phone = participant_phone, rand_dtd = randomization_dtd) %>%
  select(-gender, -record_id)

# Set date format, gender, and status id
data %<>%
  # MM/DD/YYYY
  mutate(rand_dtd = format(rand_dtd, "%m/%d/%Y")) %>%
  mutate(arm = case_match(arm,
                          "1" ~ "hsq",
                          "2" ~ "control"),
         gender = case_match(gender_bin_m,
         1 ~ "male",
         0 ~ "female"),
         statusId = str_c("pending_contact_", arm, "_", gender)) %>%
  select(-gender_bin_m) %>%
  arrange(gender, desc(n_weeks_partic))


# Clean and organize files -----------------------------------------------------
# Set the output directory
out_dir <- "Z:/Shared/DFM/HSQ_Shared/Qualitative sub-study/04_REDCap_Imports/dyad_reports/"

# List all of the .xlsx files in the output directory to move
files_to_move <- list.files(out_dir, pattern = ".xlsx")

# For-loop to copy each file in the output directory to the archive directory
# and then remove the original to maintain a clean file.
if(length(files_to_move) > 0 ){
  for(i in files_to_move){
    # Copy file to archive directory
    file.copy(from = str_c(out_dir, i), to = str_c(out_dir, "archive/", i))

    # Remove file from the outptu directory
    file.remove(str_c(out_dir, i))
    }
  }


# Write data -------------------------------------------------------------------
# Set date object which will be used in writing the dataframe to file
# Sys.time will not work to write likely due to the semi colons, therefore
# the
date_time <- str_replace_all(sub("MST.*", "", Sys.time()), ":", "_")

# Write data to the output directory
write_xlsx(data,path = str_c(out_dir, "dyad_data_", date_time, ".xlsx"))


# %%%%%%%%%%%%%%%%%%%%%% END OF SCRIPT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
