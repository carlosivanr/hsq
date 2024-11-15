# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD CU Dept of Family Medicine
# This script was written to figure out how to work with the egor package and
# the HSQ data.

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(magrittr)
library(egor) #for diagrams
library(here) #for sharing this RProject
library(tidyverse) #for data wrangling
library(purrr)
library(network) #for diagrams
library(ggplot2) #for diagrams
library(igraph) #for diagrams

library(ggraph)
library(tidygraph)

#your API token
.token <- Sys.getenv("HSQ_api")
# ------------------------------------------------------------------------------

# Step 1: Get the PNA data from redcap repor -----------------------------------
#pull report "ACTION: Needs Network Diagram Baseline"
url <- "https://redcap.ucdenver.edu/api/"
formData <- list("token"=.token,
                 content='report',
                 format='csv',
                 report_id='98678',
                 csvDelimiter='',
                 rawOrLabel='label',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 returnFormat='json'
)
response <- httr::POST(url, body = formData, encode = "form")

# Assign response content to data frame-----------------------------------------
pna_data <- httr::content(response)

# From here, go through the Wrangle PNA function -------------------------------
# Filter data to get one network
pna <-
  pna_data %>%
    filter(record_id == 1)

# Pre specified parameters, these indicate column indexes of where the names
# are at and where the ties are at.
namerange = 2:26
tierange = 502:801


# wrangle pna data -------------------------------------------------------------
# Main purpose is to create a filtered egoR object from RedCap data


# Create pna2 ------------------------------------------------------------------
# Input is redcap data
# Output is pna2 which is an input to one_file_to_egor()
# Purpose is twofold:
#   1: filter out rows w incomplete data, by counting nas, and keeping
#       rows where the na count is 0 in the namerange and tierange columns
#   2: convert the values in alttobacco and altsmoke columns from checked/unchecked
#       to Yes/No
# Currently, pna data is one row of data for one patient
pna2 <-
  pna %>%

  rowwise() %>%
  # This may only be needed for those that do not have a diagram yet
  # Could also just be for the specific report and that column isn't in this
  # report
  select(-starts_with("network_diagram_")) %>%

  # get rid of people who didn't name an alter or any alter-alter ties
  # count the number of nas in name range and tie range, names are nodes
  # ties are the edges. If the sum of nas is 0, then it means all data are complete
  # *** Consider using alter1:alter25, instead of name range, and consider
  # using m1_2:m24_25; m questions ask the degree to which 1 and 2 know each
  # other all the way through to the degree that alter 24 and 25 know each
  # other and all possible pairwise combinations in between
  filter(sum(is.na(across(all_of(namerange)))) == 0 &
           sum(is.na(across(all_of(tierange))), na.rm = TRUE) == 0) %>%

  # Reformats the response to yes or no for alt tobacco and smoke questions
  # these questions ask the degree of the alter using tobacco or smoking
  # products
  mutate(across(c(starts_with("alttobacco_a"), starts_with("altsmoke_a")),
                ~case_when(.x == "Checked" ~ "Yes",
                           .x == "Unchecked" ~ "No")))

# Create et --------------------------------------------------------------------
# Parameters from Camille's Network development script
  # pna <- needs_diagram_baseline

  # namerange <- 2:26
  # tierange <- 502:801
  # att_start <- "altgender_a1"
  # att_end <- "altquitnow_a25"
  # aa_start <- "m1_2"
  # a1 <- "alter1"
  # a25 <- "alter25"

# Input is pna2
# Output is et
# Purpose is to create an egoR object
# Import ego-centered network data from 'one file format'

# This code chunk results in a warning
# In onefile_to_egor(egos = pna2, ID.vars = list(ego = "record_id"),  :
# No netsize values provided, make sure to filter out invalid alter entries.
# The same warning pops up in camille's Network diagram development.Rmd script
et <- onefile_to_egor(egos = pna2,
                      ID.vars = list(ego = "record_id"),

                      # Name of Variable with the first alter attribute
                      attr.start.col = att_start,

                      # Name of variable with last alter attribute
                      attr.end.col = att_end,

                      # Number of maximum alters that were named by participants
                      max.alters = 25,

                      # Name of first alter-alter variable
                      aa.first.var = aa_start)

# Create alter_filter 2 --------------------------------------------------------
# input is et
# output is alter_filter 2 which is vector of TRUE/FALSE values
# Purpose is to take the et egoR object, convert to tibble, sort by egoID
#   Remove columns
#   Convert the name values into logical TRUE/FALSE values
#   Convert wide to long data
#   Then keep only the Value column as a vector
alter_filter2 <-
  et %>%
  as_tibble() %>%
  arrange(.egoID) %>%
  select(eval(substitute(a1)):eval(substitute(a25))) %>%
  mutate(across(.fns = ~ !is.na(.))) %>%
  as.data.frame() %>%
  tidyr::pivot_longer(cols = everything()) %>%
  pull(value)


# Create e2
# input is et
# ouput is e2, which is another egoR object that has been filtered
# Purpose feed e2 into activate(), which creates an index for each patient
#   of the source and target nodes and the weight in categorical format
#   filter rows according to alter_filter2
#   feed resulting data into activate, along with aatie
#   activate() turns object into dataframe
e2 <-
  et %>%
  activate(alter) %>%
  filter(alter_filter2) %>%
  activate(aatie) %>%
  filter(weight != "")

# Add variables and modify variables in egoR object-----------------------------
#add alter names to the alter data
e2$alter$aname <- c(t(e2$ego[,2:26]))

# e2$aatie$weight2 <- ifelse(e2$aatie$weight == "Not at all likely", 0.01, #set to zero
#                            ifelse(e2$aatie$weight == "Somewhat likely", 1, 2))

# aatie becomes only those that have a somewhat likely to not at all likely weight
e2$aatie <- e2$aatie %>%
  #show somewhat likely and very likely
  filter(weight != "Not at all likely")
# e2$aatie$weight2 <- ifelse(e2$aatie$weight == "Somewhat likely", 1, 2)
#show just very likel
#   filter(weight == "Very likely")
# e2$aatie$weight2 <- 3








# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Left off here going through the HSQ Network Diagram.Rmd script and following
# along in the redcap report and data dictionary to see what the columns mean
# or contain in terms of data




