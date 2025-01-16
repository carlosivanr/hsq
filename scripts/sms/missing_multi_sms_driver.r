# *****************************************************************************
# Missing multiple sms driver
# The purpose of this script is to render a word document in the same
# directory as the .qmd file. Then to move and rename the word document to
# the HSQ shared Egnyte directory. This script was designed to be used with
# Windows Task Scheduler.

# n.b. This script was designed to run on a machine where R 4.4.1 is set as the
# default version. However, the HSQ and renv environment is set up on R 4.2.2.
# As a result, this script modifies the PATH environment variable to place
# the path to R 4.2.2 at the top so that the document renders with the same
# version as the renv environment. Otherwise, quarto will try to render with
# 4.4.1 and produce errors because the packages in renv are for R 4.2.2.

# *****************************************************************************

# Get the current PATH environmental variables --------------------------------
current_path <- Sys.getenv("PATH")

# Add a new directory to the PATH at the top to force 4.2.2 instead of 4.4.1 --
new_path <- paste("C:\\Program Files\\R\\R-4.2.2\\bin\\x64",
                  current_path,
                  sep = ";")

# Set the new PATH ------------------------------------------------------------
Sys.setenv(PATH = new_path)

# Render the .qmd file to a word document -------------------------------------
quarto::quarto_render("C:/Users/rodrica2/OneDrive - The University of Colorado Denver/Documents/DFM/projects/hsq/scripts/sms/HSQ-missing-multiple-SMS.qmd") # nolint

# Copy the word document to the Egnyte directory ------------------------------
# Seems to be a problem here when trying to copy the rendered file to the
# Egnyte directory
from_file <- "C:/Users/rodrica2/OneDrive - The University of Colorado Denver/Documents/DFM/projects/hsq/scripts/sms/HSQ-missing-multiple-SMS.docx"                # nolint

date_today <- format(Sys.Date(), "%Y%m%d")


to_file <- paste0("C:\\Users\\rodrica2/OneDrive - The University of Colorado Denver\\Documents\\DFM\\projects\\hsq\\hsq_shared\\sms\\HSQ-missing-multiple-SMS-", date_today, ".docx")


file.copy(from_file, to_file, overwrite = TRUE)


# Set the PATH back to its original form --------------------------------------
Sys.setenv(PATH = current_path)