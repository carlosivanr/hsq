# Set .token ------------------------------------------------------------  
# Option 1: Seti individual credentials as separate environment variables
# Retrieves API token based on the name of the enviornment variable
.token <- Sys.getenv("HSQ_api")

# Option 2: Store all credentials in one file and set one environment
# variable to the path of the file

# Create credential file with REDCapR::create_credential_local() 
# fill out the file with the necessary URL, tokens, and project IDs
# Set the path to the credential file as a system environment variable 

# Set the path to the credential file
path_credential <- Sys.getenv("redcap_credentials")

# Set the project ID
project_id <- 11111

# Retrieve the API token dependent on the path to the credential file
# and a project specific ID
.token <- REDCapR::retrieve_credential_local(
  path_credential,
  project_id,
  check_url = TRUE,
  check_username = FALSE,
  check_token_pattern = TRUE,
  username = NA_character_
)$token



# Set URL for upload/download ----------------------------------------
url <- "https://redcap.ucdenver.edu/api/"


# Upload to REDCap ---------------------------------------------------
REDCapR::redcap_upload_file_oneshot(file_name = file_to_upload,
                                    record = i,
                                    field = "red_cap_fieldname",
                                    redcap_uri = url,
                                    token = .token)


# Additional details found in https://cran.r-project.org/web/packages/REDCapR/REDCapR.pdf