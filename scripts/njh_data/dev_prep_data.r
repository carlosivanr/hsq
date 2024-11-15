library(here)
library(tidyverse)

# Read in the data extract -----------------------------------------------------
data <- readxl::read_xlsx(
  here("scripts/njh_data/Helpers Quit Study Data Extract 10.10.24.xlsx"),
  skip = 1
)

# Get the names from the data extract of the columns that need question labels
col_names <- data.frame(question_id = names(
  data %>%
    select(`AI 1`:`UT 12`)
)) %>%
  mutate("MDS Question Id" = question_id,
         "Question Text" = NA_character_) %>%
  select(-question_id) %>%
  as_tibble()

# Import the Code book for the data extract ------------------------------------
# Set the sub directory to the codebooks director
sub_dir <- here("scripts/njh_data/codebooks")

# Get a list of all files
files <- data.frame(file = dir(sub_dir))

# Create a state column
files <- files %>%
  mutate(state = str_sub(file, 1, 2))

# Loop through all files
for (i in seq_along(1:dim(files)[1])) {
  print(i)
  # Read in an INTAKE exel file and remove columns that start with
  # "..." since these columns will be full of NAs
  temp <- readxl::read_xlsx(
    str_c(sub_dir, "/", files$file[i]),
    skip = 9 # Skip the first 9 rows
  ) %>%
    select(-starts_with("..."))

  # Set only the rows that need to be filled
  to_be_filled <- col_names %>%
    filter(is.na(`Question Text`)) %>%
    select(-`Question Text`) %>%
    as_tibble()

  if (to_be_filled %>% nrow() > 0) {
    # Get the column labels from the codebook that are in the column names
    # of the data extract

    # Some questions have the same stem but slightly different text, this just
    # sets up an algorithm to take one of the many until further guidance
    col_labels <- temp %>%
      filter(`MDS Question Id` %in% to_be_filled$`MDS Question Id`) %>%
      select("MDS Question Id", "Question Text") %>%
      distinct() %>%
      group_by(`MDS Question Id`) %>%
      slice_head() %>%
      ungroup()

    output <- left_join(to_be_filled, col_labels, by = c("MDS Question Id"))

    output <- output %>%
      drop_na(`Question Text`)

    col_names <<-
      bind_rows(output,
                (col_names %>%
                   filter(!`MDS Question Id` %in% output$`MDS Question Id`)))
  }

}




# SEARCH EACH CODE BOOK FILE FOR CODES -----------------------------------------
# Check that the code books nor the eligibility questions have the remaining
# MDS question ids.
for (i in seq_along(1:dim(files)[1])) {
  print(files$file[i])
  # Read in an INTAKE exel file and remove columns that start with
  # "..." since these columns will be full of NAs
  temp <- readxl::read_xlsx(
    str_c(sub_dir, "/", files$file[i]),
    skip = 9 # Skip the first 9 rows
  ) %>%
    select(-starts_with("..."))

  temp <- temp %>%
    filter(grepl('UT 12', `MDS Question Id`)) %>%
    select("MDS Question Id")

  print(temp)
}

# Merge in the participants State to determine which questions to get

# Col_names that are still missing
col_names %>%
  filter(is.na(`Question Text`)) %>%
  select("MDS Question Id") %>%
  print(n = 26)
