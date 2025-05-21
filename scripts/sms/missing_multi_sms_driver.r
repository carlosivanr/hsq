# *****************************************************************************
# Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine
#
# Missing multiple sms driver
#
# The purpose of this script is to render a quarto file to a word document in
# the same directory as the .qmd file. Then to move and rename the word
# document to the HSQ shared Egnyte directory. This script was designed to be
# used with run_missing_multi_sms.bat which is set up to run on a regular basis
# in Windows Task Scheduler.

# n.b. This script was designed to run on a machine where R 4.4.1 is set as the
# default version. However, the HSQ renv environment is set up on R 4.2.2. As a
# result, this script modifies the PATH environment variable to place the path
# to R 4.2.2 at the top so that the document renders with the same version as
# the renv environment. Otherwise, quarto will try to render with 4.4.1 and
# will produce errors because the packages in renv are for R 4.2.2 not 4.4.1.
# *****************************************************************************

# Get the current PATH environmental variables --------------------------------
current_path <- Sys.getenv("PATH")

# Add a new directory to the PATH at the top to force 4.2.2 instead of 4.4.1 --
# This is set because of multiple versions of R and needing to set 4.2.2 for
# the report to render properly
new_path <- paste("C:\\Program Files\\R\\R-4.2.2\\bin\\x64",
                  current_path,
                  sep = ";")

# Set the new PATH ------------------------------------------------------------
Sys.setenv(PATH = new_path)

# set qmd file
qmd_file <- "C:/Users/rodrica2/OneDrive - The University of Colorado Denver/Documents/DFM/projects/hsq/scripts/sms/HSQ-missing-multiple-SMS.qmd" # nolint

# Render the .qmd file to a word document -------------------------------------
quarto::quarto_render(qmd_file) # nolint

# Copy and rename the word document to the Egnyte directory -------------------
# set the source file
from_file <- "C:\\Users\\rodrica2\\OneDrive - The University of Colorado Denver\\Documents\\DFM\\projects\\hsq\\scripts\\sms\\HSQ-missing-multiple-SMS.docx" # nolint

# Capture today's date to append to the new file name --------------------------
date_today <- format(Sys.Date(), "%Y%m%d")


# ******************************************************************************
# !!! Experimental: Upload file to REDCap File Repository instead of Egnyte !!!
# Find the folder_id by using the API Playground on the REDCap website. Select
# API Method as Export a list of files/folder, copy the R code and execute it
# to determine the folder id of the destination directory.

# Set the name of the renamed file with the date appended
renamed_file <- paste0("C:\\Users\\rodrica2\\OneDrive - The University of Colorado Denver\\Documents\\DFM\\projects\\hsq\\scripts\\sms\\HSQ-missing-multiple-SMS", date_today, ".docx")

# Copy from_file to renamed_file
file.copy(from_file, renamed_file, overwrite = TRUE)

# Set token and URL
.sms_token <- Sys.getenv("HSQ_sms")
url <- "https://redcap.ucdenver.edu/api/"

# Specify formData
formData <- list(token=.sms_token,
                 action='import',
                 content='fileRepository',
                 folder_id = "732",

                 returnFormat='csv',
                 file=httr::upload_file(renamed_file)
)

# Upload the file
response <- httr::POST(url, body = formData, encode = "multipart")
# result <- httr::content(response)
# print(result)
# ******************************************************************************


# Set the destination file with appended date
to_file <- paste0("Z:\\Shared\\DFM\\HSQ_Shared\\Reporting\\2 week SMS report\\HSQ-missing-multiple-SMS-", date_today, ".docx")

# Move the file
file.copy(from_file, to_file, overwrite = TRUE)

# Set the PATH back to its original form --------------------------------------
# Sys.setenv(PATH = current_path)
