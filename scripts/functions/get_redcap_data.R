# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine
# Get RedCap Data
#
# Description:
# This function was designed to automatically get/update all RedCap data from  
# the PATHWEIGH practice member survey.
#
# Requirements:
# This function relies on the REDCapR package.
# install.packages("REDCapR")
#           OR
# install.packages("remotes")                       # Install 'remotes' version
# remotes::install_github(repo="OuhscBbmc/REDCapR") # Install development version
#
# This function also requires a credentials file that is a comma separated
# file containing the RedCap URI, user_name, project ID, and token
# see https://ouhscbbmc.github.io/REDCapR/articles/workflow-read.html for more
# details on setting up a credentials file.
#
# Users may have to ask for permissions in RedCap to be able to get a token.
#
# *** Could use updated arguments for specifying columns(fields) or observations
# (records) by selecting and filtering.
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  
get_redcap_data <- function(pid){
  # Set file path parts --------------------------------------------------------
  # *** These would potentially be modified in an OS specific way
  prefix <- "C:/Users/"
  
  user <- Sys.getenv("USERNAME")
  
  docs <- 
    "/OneDrive - The University of Colorado Denver/Documents/redcap_credentials/credentials"
  
  # Set path to credentials file -----------------------------------------------
  path_credential <- paste0(prefix, user, docs)
  
  
  # Create credential object ---------------------------------------------------
  credential  <- 
    REDCapR::retrieve_credential_local(
    path_credential = path_credential,
    project_id = pid)
  
  
  # Read data ------------------------------------------------------------------
  data <-
    REDCapR::redcap_read(
      redcap_uri = credential$redcap_uri,
      export_survey_fields = TRUE,
      token = credential$token)$data
  }