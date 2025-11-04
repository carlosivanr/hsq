library(tidyverse)
library(readxl)

# Specify the path to the data file
proj_root <- 'C:\\Users\\rodrica2\\OneDrive - The University of Colorado Denver\\Documents\\DFM\\projects\\hsq'

sub_dir <- '\\scripts\\njh_data\\codebooks'

data_dir <- '\\scripts\\njh_data\\Helpers Quit Study Data Extract 01.06.25.xlsx'

file_path <- str_c(proj_root, data_dir)

data <- read_xlsx(file_path, skip = 1)

# Question Id Map
data.frame(
  q_id = (data %>%
            select(`NJ 31`:`UT 9`) %>%
            names()),
  q_txt = NA)



# -----------------------------------------------------------------------------
# Read in the codebooks
codebooks <- list.files(path = str_c(proj_root, sub_dir))


for (file in codebooks) {
  # print(file)
  path <- str_c(proj_root, sub_dir,  "\\", file)
  temp <- read_xlsx(path, skip = 9)

  temp %>%
    filter(`MDS Question Id` %in% data$)


}