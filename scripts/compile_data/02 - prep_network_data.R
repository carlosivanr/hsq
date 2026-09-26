# /////////////////////////////////////////////////////////////////////////////
# Carlos Rodriguez, PhD. CU Dept. of Family Medicine
# 09/02/2025

# Prepare Network Data

# The following script prepares the baseline and 12month network measures from 
# the social network portion of the HSQ Participant Management RedCap project.

# Processed data is output to SAS HSQ/analyses_sas/data directory

# Network Measures Terms:
# MEASURES THAT REPRESENT PROPERTIES OF THE NETWORKS
# density - Proportion of all possible edges that are present
# components - Number of disconnected subnetworks
# betweenness centralization - Whether bridging is concentrated in a few nodes
# degree centralization - How unequal the node degrees are across the network

# MEASURES THAT REPRESENT PROPERTIES OF THE NODES WITHIN A NETWORK
# betweenness centrality - How often a node lies on shortest paths between 
# other nodes. Identifies influential people in social networks, if normalized
# values lies between 0 and 1, otherwise range is from 0 to no fixed upper 
# limit. Larger values represent higher potential influence. These are people
# who tend to form bridges between other groups of people. Also known as 
# brokers.

# closeness centrality - Defined as the inverse of the total shortest-path 
# distance from a node to all others. Higher values indicate shorter average
# distances to other nodes. These individuals are well-positioned. If 
# normalized values will range from 0 to 1, otherwise ranges from 0 to no
# fixed upper limit.

# degree centrality - Defined as the number of direct connections for each node
# Ranges from 0 to n-1, where n is the number of nodes (25 alters) unless 
# normalized. Measures the number of neighbors.

# /////////////////////////////////////////////////////////////////////////////

library(tidyverse)
library(gtsummary) 
library(here) 
library(egor) 
library(igraph)


# Set in environmental variables in Windows
# Token generated in RedCap once API access is granted
.token <- Sys.getenv("HSQ_api")

# Set Redcap URL
url <- "https://redcap.ucdenver.edu/api/"

 
# load the field names used to cull the columns from Camille's
# network visualization script.
field_names <- read_csv(
  here("scripts/network_measures/data/field_names.csv"),
  show_col_types = FALSE
)

field_names <- field_names %>%
    filter(!grepl("diagram", field_name, ignore.case = TRUE)) %>%
    filter(!grepl("patientend", field_name, ignore.case = TRUE)) %>%
    filter(field_name != "baseline_complete")

 
# Load 0mo survey data
# Set formData, contains token and other parameters
# Set the report_id to pull from
formData <- list("token"=.token,
                 content='report',
                 format='csv',
                 report_id='112310',
                 csvDelimiter='',
                 rawOrLabel='label',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 returnFormat='json'
)

# Create response object using formData
response <- httr::POST(url, body = formData, encode = "form")

# Create needs_qual_diagram which is the main data frame for generating
# network diagrams
survey_0mo <- httr::content(response, show_col_types = FALSE)

# Set aside the psychological measures to assess straightlining as a data qa 
# step
psy_0mo <- survey_0mo %>%
  select(record_id, arme_1:se5)

# Create a separate data frame with the alter and tie cols
survey_0mo <- survey_0mo %>%
  select(hsqid, gender, age, arm, all_of(field_names$field_name)) %>%
  mutate(event_name = "0mo")

 
# Load 12mo survey data
# Set formData, contains token and other parameters
# Set the report_id to pull from
formData <- list("token"=.token,
                 content='report',
                 format='csv',
                 report_id='112314',
                 csvDelimiter='',
                 rawOrLabel='label',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 returnFormat='json'
)

# Create response object using formData
response <- httr::POST(url, body = formData, encode = "form")

# Create needs_qual_diagram which is the main data frame for generating
# network diagrams
survey_12mo <- httr::content(response, show_col_types = FALSE)

# Modify the names of the survey 12m timepoint
survey_12mo_names <- names(survey_12mo)

# Replace the '_12m' string in the names
new_names <- str_replace(survey_12mo_names, "_12m", "")

colnames(survey_12mo) <- new_names

# Create a separate data frame with the alter and tie cols
survey_12mo <- survey_12mo %>%
select(hsqid, all_of(field_names$field_name)) %>%
mutate(event_name = "12mo")
 
# Count the number of alters entered at the baseline survey timepoint
n_alters_0mo <- survey_0mo %>%
select(alter1:alter25) %>%
rowwise() %>%
mutate(n_missing = sum(is.na(c_across(everything())))) %>%
ungroup() %>%
mutate(n_alters = 25 - n_missing) %>%
select(n_alters)

survey_0mo <- bind_cols(survey_0mo, n_alters_0mo)

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
# When 12mo data is ready to be analyzed, uncomment line 155 to bind the
#      the 0mo data and the 12mo data and process all rows together with the
#      same algorithm. 
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 

 # ////////////////////////// DATA PROCESSING /////////////////////////// 
# Clean data, create a categorical age variable for homophily measures
data <- bind_rows(
    survey_0mo,
    survey_12mo # *** WHEN 12mo DATA IS AVAIALABLE, UNCOMMENT ***
    ) %>% 
  group_by(record_id) %>%
  fill(gender, age, arm) %>%
  ungroup() %>%
  mutate(
    gender = case_match(
      gender,
      ",Man (including transman and transmasculine)" ~ "Man (including transman and transmasculine)",
      "Prefer to self-describe (non-binary, gender queer) please specify below" ~ "Other", 
      .default = gender)
  ) %>% 
  mutate(across(starts_with("altgender_"), 
    ~ case_match(
      .x,
      "Different Gender Identity (including non-binary and gender queer)" ~ "Other",
      .default = .x))
  ) %>%
  mutate(age_cat = ifelse(age >= 18 & age <= 64, "18 - 64", "65 or older")) %>%
  mutate(arm = ifelse(arm == "2 - HSQ training - end of participation", "Control", "Intervention")) %>%
  mutate(ego_id = str_c(record_id, "_", event_name)) %>%
  select(hsqid, gender, arm, age_cat, ego_id, all_of(field_names$field_name))


# Parameters -----------------------------------------------

# these cols will represent alter names, 25 total in a data frame
# that will be culled
namerange <- 2:26

# these cols will represent the ties or links, up to 300 because
# 25 * (25 - 1) / 2 = 300.
tierange <- 502:801

# Clean Input Data -----------------------------------------
# Create a data frame where only the relevant columns and rows are retained. The
# filter verb should drop any rows where any of the alter names or alter-alter
# ties are missing, and should negate the need for a mask downstream. n.b. add
# additional columns after the namerange and tierange otherwise the numerical
# indexing is disrupted.
filtered_ego_data <- data %>%
  select(ego_id, alter1:m24_25, gender, age_cat) %>%
  filter(
    if_all(all_of(namerange), ~ !is.na(.x)) &
    if_all(all_of(tierange), ~ !is.na(.x))) %>%
  mutate(across(
    .cols = c(starts_with("alttobacco_a"), starts_with("altsmoke_a")),
    .fns  = ~ case_when(
      .x == "Checked" ~ "Yes",
      .x == "Unchecked" ~ "No",
      TRUE              ~ NA_character_
    )
  ))


# Convert to egor Object -----------------------------------
# Specify the start and stop cols of the alter attributes such as gender, age,
# smoking status, etc. The input data is expected to be arranged in such a
# manner that aa.first.var through the last column all contain the
# alter-to-alter tie block of cols. n.b. will Create a warning, but it's benign
# because invalid entries will be filtered out in a subsequent step.
ego_network_long <- invisible(
  onefile_to_egor(
    egos            = filtered_ego_data,
    ID.vars         = list(ego = "ego_id"),
    attr.start.col  = "altgender_a1",
    attr.end.col    = "altquitnow_a25",
    max.alters      = 25,
    aa.first.var    = "m1_2"
  )
)


# Identify Valid Alters ------------------------------------
# Take the long ego network objected and flatten it out to make it a
# functionally wide tibble, then sort, and select the relevant columns of the
# alter names in order to determine if a value is present. Finally, convert back
# to long to use as a mask.
alter_mask <- ego_network_long %>%
  as_tibble() %>%
  arrange(.egoID) %>%
  select(alter1:alter25) %>%
  mutate(across(everything(), ~ !is.na(.))) %>%
  pivot_longer(cols = everything()) %>%
  pull(value)

# Filter Alters and Alter-Alter Ties -----------------------
# In this step, first dichotomize the ties from three levels to two levels. Only
# ties which are "Somewhat likely" or "Very likely" are retained as a measure of
# tie/link.
final_ego_network <- ego_network_long %>%
  activate(alter) %>%
  filter(alter_mask) %>%
  activate(aatie) %>%
  filter(weight != "" & weight != "Not at all likely")


# Add Alter Names ------------------------------------------
final_ego_network$alter$aname <- c(t(final_ego_network$ego[, 2:26]))


# Filter the primary data frame to those that hade valid filtered
# ego network 
data <- data %>%
  filter(ego_id %in% filtered_ego_data$ego_id)

 
# Parallel Vectors -----------------------------------------
# Create two vectors of the same length to iterate with purrr. These vectors
# will be used with functions from the igraph package.
graphs <- as_igraph(final_ego_network, include.ego = FALSE)
ego_ids <- final_ego_network$ego$.egoID

# ///////////////////////////////////////////////////////////////////// 
#                        NETWORK LEVEL MEASURES                         
# ///////////////////////////////////////////////////////////////////// 

# ////////////////////////////// Density //////////////////////////////  
density_results <- ego_density(final_ego_network)


# //////////////////////// Number of components ////////////////////////
# For each x and y, compute the number of components and bind to the ego id,
# collect all results, and output to a data frame. Since two pacakges are
# used to compute the network metrics, perform harmonization on the ego column
# name via rename().
components_results <- map2_dfr(graphs, ego_ids, ~ {
  data.frame(ego = .y, n_components = components(.x)$no)
}) %>%
rename(".egoID" = "ego")


# ///////////////////// Betweenness Centralization /////////////////////
 # For each x and y, compute betweeness centralization and bind to the ego
# id, collect all results and output to a data frame. Since two pacakges are
# used to compute the network metrics, perform harmonization on the ego column
# name via rename().
btw_centr_results <- map2_dfr(graphs, ego_ids, ~ {
  data.frame(ego = .y, btw_centralization = centr_betw(.x)$centralization)
}) %>%
rename(".egoID" = "ego")


# /////////////////////// Degree Centralization ////////////////////////
# For each x and y, compute degree centralization and bind to the ego id,
# collect all results and output to a data frame. Since two pacakges are used to
# compute network metrics, perform harmonization on the ego column name via
# rename().
deg_centr_results <- map2_dfr(graphs, ego_ids, ~ {
  data.frame(ego = .y, deg_centr = centr_degree(.x)$centralization)
}) %>%
rename(".egoID" = "ego")



# ///////////////////////////////////////////////////////////////////// 
#                         NODE LEVEL MEASURES                           
# ///////////////////////////////////////////////////////////////////// 

# //////////////////////  Betweenness Centrality ////////////////////// 
# Betweeness centrality (not the same as centralization) graphs do not include
# the ego, each graph is comprised of 25 elements, ties between alters only. For
# each participant/ego, calculate the betweenness centrality measure and capture
# the smoking status of each alter and store in a list. Results in a list with
# length of number of participants with valid survey data.

# Capture the smoking status of the alter that has the highest node centrality
# value. In cases of ties, a series of variables will be created. Capture the
# smoking status of a randomly selected alter from the ties, set the smoking
# status to 1 if any of the tied alters report smoking, and then capture the
# proportion of smokers among those tied.

btw_centrality_L <- map2(graphs, ego_ids, ~ {
  df = betweenness(
    .x,
    v = V(.x),
    directed = FALSE,
    weights = NA,
    normalize = FALSE,
    cutoff = -1) %>%
  as_tibble() %>%
  mutate(alter = 1:nrow(.)) %>%
  arrange(desc(value)) %>%
  mutate("ego_id" = .y) %>%
  mutate(alter = str_c("alttobacco_a", alter, "___1"))
  
  # Get the number of alters that tied for the max value
  tied_rows <- df %>% 
    filter(value == max(value, na.rm = TRUE))

  # Pull the names of the columns needed to extract from data
  alter_ids <- df %>% 
    pull(alter)

  # Capture the smoking status of each alter, ordered by the
  # alter ids using the dataframe "data" in the workspace and
  # convert to long format.
  alt_smoke_status <- data %>%
    filter(ego_id == .y) %>%
    select(all_of(alter_ids)) %>%
    pivot_longer(cols = everything(), names_to = "alter", values_to = "alt_smoke") %>%
    mutate(alt_smoke = ifelse(alt_smoke == "Checked", 1, 0))

  # Join the smoking status of the alters to df
  df <- left_join(df, alt_smoke_status, by = "alter")

  # Smoking status of the alter with the highest centrality value
  # If there are ties, set to NA and exclude from analyses
  if (nrow(tied_rows) == 1) {
    alt_smoke_max <- df %>%
      slice_max(value) %>%
      pull(alt_smoke)
  } else {
    alt_smoke_max = NA
  }

  # Smoking status of the alter with the highest centrality value
  # select a random alter and use that smoking status
  if (nrow(tied_rows) == 1) {
    alt_smoke_rand <- df %>%
      slice_max(value) %>%
      pull(alt_smoke)
  } else {
    alt_smoke_rand <- df %>%
      slice_max(value) %>%
      slice_sample(n = 1) %>%
      pull(alt_smoke)
  }

  # Smoking status of alter with the highest centrality value
  # if there are ties, and 1 person in the tied group smokes, then set smoking
  # status to 1.
  if (nrow(tied_rows) == 1) {
    alt_smoke_any <- df %>%
      slice_max(value) %>%
      pull(alt_smoke)
  } else {
    alt_smoke_any <- df %>%
      slice_max(value) %>%
      summarise(sum_alt_smoke = sum(alt_smoke)) %>%
      mutate(sum_alt_smoke = ifelse(sum_alt_smoke > 0, 1, sum_alt_smoke)) %>%
      pull(sum_alt_smoke)

  }

  # get the proportion of alters that smoke amongst those with the highest 
  # centrality value
  prop <- df %>% 
    slice_max(value) %>% 
    summarise(prop = mean(alt_smoke)) %>% 
    pull(prop)

  # Create a list that will be the functions output
  list(
    data = df, 
    btw_ties = if (nrow(tied_rows) > 1) nrow(tied_rows) else 0,
    btw_alt_smoke_max = alt_smoke_max,
    btw_alt_smoke_rand = alt_smoke_rand,
    btw_alt_smoke_any = alt_smoke_any,
    btw_prop_alt_smoke = prop
  )

})

# Create a data frame listing the number of ties for node centrality for each 
# ego then filter to see which Ids have ties, size of df will also give the 
# number of participants with ties.
n_tied_btw_centrality <- enframe(
  map_dbl(btw_centrality_L, ~ .x$btw_ties),
  name = "ego_id",
  value = "n_tied"
)

vars <- c("btw_ties", "btw_alt_smoke_max", "btw_alt_smoke_rand", "btw_alt_smoke_any", "btw_prop_alt_smoke")

btw_centr_df <- imap_dfr(
  btw_centrality_L,
  ~ c(list(ego_id = .y), .x[vars]) %>% as_tibble()
)

# View data from those who have 25 nodes tied for max centrality
btw_centr_df %>%
  filter(btw_ties == 25)


 
# ///////////////////////////// Node Degree //////////////////////////// 
degree_L <- map2(graphs, ego_ids, ~ {
    df = degree(
      .x,
      v = V(.x),
      mode = "all",
      loops = FALSE,
      normalize = FALSE) %>%
    as_tibble() %>%
    mutate(alter = 1:nrow(.)) %>%
    arrange(desc(value)) %>%
    mutate("ego_id" = .y) %>%
    mutate(alter = str_c("alttobacco_a", alter, "___1"))

  # Get the number of alters that tied for the max value
  tied_rows <- df %>% 
    filter(value == max(value, na.rm = TRUE))

  # Pull the names of the columns needed to extract from data
  alter_ids <- df %>% 
    pull(alter)

  # Capture the smoking status of each alter, ordered by the
  # alter ids, requires data in the workspace
  alt_smoke_status <- data %>%
    filter(ego_id == .y) %>%
    select(all_of(alter_ids)) %>%
    pivot_longer(cols = everything(), names_to = "alter", values_to = "alt_smoke") %>%
    mutate(alt_smoke = ifelse(alt_smoke == "Checked", 1, 0))

  # Join the smoking status of the alters to df
  df <- left_join(df, alt_smoke_status, by = "alter")

  # Smoking status of the alter with the highest centrality value
  # If there are ties, set to NA and exclude from analyses
  if (nrow(tied_rows) == 1) {
    alt_smoke_max <- df %>%
      slice_max(value) %>%
      pull(alt_smoke)
  } else {
    alt_smoke_max = NA
  }

  # Smoking status of the alter with the highest centrality value
  # select a random alter and use that smoking status
  if (nrow(tied_rows) == 1) {
    alt_smoke_rand <- df %>%
      slice_max(value) %>%
      pull(alt_smoke)
  } else {
    alt_smoke_rand <- df %>%
      slice_max(value) %>%
      slice_sample(n = 1) %>%
      pull(alt_smoke)
  }

  # Smoking status of alter with the highest centrality value
  # if there are ties, and 1 person in the tied group smokes,
  # then set smoking status to 1.
  if (nrow(tied_rows) == 1) {
    alt_smoke_any <- df %>%
      slice_max(value) %>%
      pull(alt_smoke)
  } else {
    alt_smoke_any <- df %>%
      slice_max(value) %>%
      summarise(sum_alt_smoke = sum(alt_smoke)) %>%
      mutate(sum_alt_smoke = ifelse(sum_alt_smoke > 0, 1, sum_alt_smoke)) %>%
      pull(sum_alt_smoke)

  }

  # get the proportion of alters that smoke amongst those with the highest 
  # centrality value
  prop <- df %>% 
    slice_max(value) %>% 
    summarise(prop = mean(alt_smoke)) %>% 
    pull(prop)

  # Create a list that will be the functions output
  list(
    data = df, 
    deg_ties = if (nrow(tied_rows) > 1) nrow(tied_rows) else 0,
    deg_alt_smoke_max = alt_smoke_max,
    deg_alt_smoke_rand = alt_smoke_rand,
    deg_alt_smoke_any = alt_smoke_any,
    deg_prop_alt_smoke = prop
  )

})

# Create a data frame listing the number of ties for node centrality for each 
# ego then filter, to see which Ids have ties, size of df will also give the 
# number of participants with ties.
n_tied_degree <- enframe(
  map_dbl(degree_L, ~ .x$deg_ties),
  name = "ego_id",
  value = "n_tied"
) 

vars <- c("deg_ties", "deg_alt_smoke_max", "deg_alt_smoke_rand", "deg_alt_smoke_any", "deg_prop_alt_smoke")

degree_df <- imap_dfr(
  degree_L,
  ~ c(list(ego_id = .y), .x[vars]) |> as_tibble()
)


# ///////////////////////////// Closeness ////////////////////////////// 
# *** displays WARNINGS due to data QA
closeness_L <- map2(graphs, ego_ids, ~ {
    df = closeness(
      .x,
      vids = V(.x),
      mode = "all",
      weights = NA,
      normalize = FALSE,
      cutoff = -1) %>%
    as_tibble() %>%
    mutate(alter = 1:nrow(.)) %>%
    arrange(desc(value)) %>%
    mutate("ego_id" = .y) %>%
    mutate(alter = str_c("alttobacco_a", alter, "___1"))

  # *** Displays warnings, when all values of closeness are missing like 
  # indexes 44, 96, 275, etc. max_closeness is NaN in closeness_df. The indexes
  # can be used to look up the ego_id/record_id. These individuals likely
  # straightlined the strength of their responses.
  tied_rows <- df %>% 
    filter(value == max(value, na.rm = TRUE))

  # Pull the names of the columns needed to extract from data
  alter_ids <- df %>% 
    pull(alter)

  # Capture the smoking status of each alter, ordered by the
  # alter ids using the dataframe "data" in the workspace and
  # convert to long format.
  alt_smoke_status <- data %>%
    filter(ego_id == .y) %>%
    select(all_of(alter_ids)) %>%
    pivot_longer(cols = everything(), names_to = "alter", values_to = "alt_smoke") %>%
    mutate(alt_smoke = ifelse(alt_smoke == "Checked", 1, 0))

  # Join the smoking status to df
  df <- left_join(df, alt_smoke_status, by = "alter")

  # Smoking status of the alter with the highest centrality value
  # If there are ties, set to NA and exclude from analyses
  if (nrow(tied_rows) == 1) {
    alt_smoke_max <- df %>%
      slice_max(value) %>%
      pull(alt_smoke)
  } else {
    alt_smoke_max = NA
  }

  # Smoking status of the alter with the highest centrality value
  # select a random alter and use that smoking status
  if (nrow(tied_rows) == 1) {
    alt_smoke_rand <- df %>%
      slice_max(value) %>%
      pull(alt_smoke)
  } else {
    alt_smoke_rand <- df %>%
      slice_max(value) %>%
      slice_sample(n = 1) %>%
      pull(alt_smoke)
  }

  # Smoking status of alter with the highest centrality value
  # if there are ties, and 1 person in the tied group smokes,
  # then set smoking status to 1.
  if (nrow(tied_rows) == 1) {
    alt_smoke_any <- df %>%
      slice_max(value) %>%
      pull(alt_smoke)
  } else {
    alt_smoke_any <- df %>%
      slice_max(value) %>%
      summarise(sum_alt_smoke = sum(alt_smoke)) %>%
      mutate(sum_alt_smoke = ifelse(sum_alt_smoke > 0, 1, sum_alt_smoke)) %>%
      pull(sum_alt_smoke)

  }

  # get the proportion of alters that smoke amongst those with the highest 
  # centrality value
  prop <- df %>% 
    slice_max(value) %>% 
    summarise(prop = mean(alt_smoke)) %>% 
    pull(prop)

  # Create a list that will be the functions output
  list(
    data = df, 
    clos_ties = if (nrow(tied_rows) > 1) nrow(tied_rows) else 0,
    clos_alt_smoke_max = alt_smoke_max,
    clos_alt_smoke_rand = alt_smoke_rand,
    clos_alt_smoke_any = alt_smoke_any,
    clos_prop_alt_smoke = prop
  )

})

# Create a data frame listing the number of ties for node centrality for each 
# ego then filter, to see which Ids have ties, size of df will also give the 
# number of participants with ties.
n_tied_closeness <- enframe(
  map_dbl(closeness_L, ~ .x$clos_ties),
  name = "ego_id",
  value = "n_tied"
) 

vars <- c("clos_ties", "clos_alt_smoke_max", "clos_alt_smoke_rand", "clos_alt_smoke_any", "clos_prop_alt_smoke")

closeness_df <- imap_dfr(
  closeness_L,
  ~ c(list(ego_id = .y), .x[vars]) |> as_tibble()
)


# This section is to create tables for the individual .xlsx files used to show 
# the differences between the different approaches of capturing the smoking
# staus of the later with the highes centrality value

# Summarise the different variables for each centrality measure
summarise_centrality_df <- function (df) {
  df <- btw_centr_df

  names(df) <- c("ego_id", "ties", "alt_smoke_max", "alt_smoke_rand", "alt_smoke_any", "prop_alt_smoke")

  df %>%
    select(-ego_id, -ties) %>%
    pivot_longer(cols = everything(), names_to = "type", values_to = "value") %>%
    tbl_summary(
      by = type,
      statistic = all_continuous() ~ "{min}, {max}; {mean} ({sd})" 
    )

}

# Betweenness Centrality 
summarise_centrality_df(btw_centr_df)

n_tied_btw_centrality %>% 
  filter(n_tied > 0) %>%
  select(n_tied) %>%
  mutate(n_tied = factor(n_tied)) %>%
  tbl_summary() %>%
  as_tibble()

# Degree Centrality
summarise_centrality_df(degree_df)

n_tied_degree %>% 
  filter(n_tied > 0) %>%
  select(n_tied) %>%
  mutate(n_tied = factor(n_tied)) %>%
  tbl_summary()

# Closeness Centrality
summarise_centrality_df(closeness_df)

n_tied_closeness %>% 
  filter(n_tied > 0) %>%
  select(n_tied) %>%
  mutate(n_tied = factor(n_tied)) %>%
  tbl_summary()



# ///////////////////////////// Community ////////////////////////////// 
# Community algorithm function 
get_com_measures <- function(x, .y){
  # Function to calculate community measures for various types of community 
  # detection algorithms.
  # Input is the result of the output of the membership() function
  # 1. Number of communities that have gte 50% smokers
  # 2. Number of communities that have gte 50% smokers AND the proportion of alters with daily interaction is gte 50%.
  # 3. Number of communities that have gte 50% smokers AND the proportion of alters known from work or school is gte 50%.
  # Requires the full data frame to be available in the workspace

  # Create a dataframe tabulating which community each participants alters 
  # belong to
  comm_df <- data.frame(
    alter_id = as.numeric(names(x)),
    community_id = as.vector(x))

  # Selects current tobacco smoking only, not vaping
  cols_to_select <- data.frame(
    alter = 1:25) %>%
  mutate(
    smoke = str_c("alttobacco_a", alter, "___1"), # selects if alter currently smokes tobacco
    freq = str_c("altintfreq_a", alter),
    rel = str_c("altrelation_a", alter) 
  )

  # Alter current tobacco smoking status
  alt_smoke <- data %>%
      filter(ego_id == .y) %>% # select one row per participant on each iteration
      select(all_of(cols_to_select$smoke)) %>%
      pivot_longer(cols = everything(), values_to = "alt_smoke", names_to = "alter_id") %>%
      mutate(
        alt_smoke = ifelse(alt_smoke == "Checked", 1, 0),
        alter_id = 1:25
      )

  # Alter frequency of interaction is almost daily
  alt_freq <- data %>%
      filter(ego_id == .y) %>% # select one row per participant on each iteration
      select(all_of(cols_to_select$freq)) %>%
      pivot_longer(cols = everything(), values_to = "alt_freq", names_to = "alter_id") %>%
      mutate(
        alt_freq = ifelse(alt_freq == "Almost every day", 1, 0),
        alter_id = 1:25
      )

  # Alter relationship is through work or school
  alt_rel <- data %>%
      filter(ego_id == .y) %>% # select one row per participant on each iteration
      select(all_of(cols_to_select$rel)) %>%
      pivot_longer(cols = everything(), values_to = "alt_rel", names_to = "alter_id") %>%
      mutate(
        alt_rel = ifelse(alt_rel == "Your work or School", 1, 0),
        alter_id = 1:25
      )

  # Put all data frames into a list to merge by alter id
  df_list <- list(comm_df, alt_smoke, alt_freq, alt_rel)

  result <- reduce(df_list, left_join, by = "alter_id")

  # summarise by the community id
  result_summary <- result %>%
    group_by(community_id) %>%
    summarise(
      mean_alt_smoke = mean(alt_smoke),
      mean_alt_freq = mean(alt_freq),
      mean_alt_rel = mean(alt_rel),
      .groups = "drop"
    )

  # Number of communities that have gte 50% smokers
  n_coms_smoke_50 <- result_summary %>%
    filter(mean_alt_smoke >= .50) %>%
    nrow()

  # Number of communities that have gte 50% smokers AND the proportion of alter 
  # with daily interaction is gte 50%.
  n_coms_smoke_50_freq_50 <- result_summary %>%
    filter(
      mean_alt_smoke >= .50,
      mean_alt_freq >= .50) %>%
    nrow()

  # Number of communities that have gte 50% smokers AND the proportion of alter 
  # known from work or school is gte 50%.
  n_coms_smoke_50_rel_50 <- result_summary %>%
    filter(
      mean_alt_smoke >= .50,
      mean_alt_rel >= .50) %>%
    nrow()

  return(
    c(
      n_coms_smoke_50,
      n_coms_smoke_50_freq_50,
      n_coms_smoke_50_rel_50)
  )
}

# Louvain algorithm for community detection
# Generates a data frame, one row per each participant with the modularity 
# score, the number of communities using the Louvain algorithm.
# Requires get_com_measures() function
comm_lu_df <- map2_dfr(graphs, ego_ids, ~ {
  community_lu = cluster_louvain(
    .x,
    weights = NA,
    resolution = 1)

  # Displays one value for modularity
  modularity_score <- modularity(community_lu)

  # The number of communities
  comm_len <- length(community_lu)

  # Lists what community each alter belongs to and get the community measures
  x <- membership(community_lu)
  com_measures <- get_com_measures(x, .y)

  # Create a list that will be the output
  # sics = smoking intensive communities
  data.frame(
    ego_id = .y, 
    len_comm_lu = comm_len, 
    modularity_lu = modularity_score,
    n_sics_lu = com_measures[1],
    n_sics_daily_lu = com_measures[2],
    n_sics_work_lu = com_measures[3]
    )
})

# For those where modularity is 25, set values to NA to exclude from analyses
comm_lu_df <- comm_lu_df %>%
  mutate(across(n_sics_lu:n_sics_work_lu, ~ifelse(modularity_lu == 25, NA, .x)))


# Girvan-Newman algorithm for community detection
comm_gn_df <- map2_dfr(graphs, ego_ids, ~ {
  community_gn = cluster_edge_betweenness(
    .x,
    weights = NA,
    edge.betweenness = TRUE,
    merges = TRUE,
    bridges = TRUE,
    modularity = TRUE,
    membership = TRUE)

  # Displays one value for modularity
  modularity_score <- modularity(community_gn)

  # The number of communities
  comm_len <- length(community_gn)

  # Lists what community each alter belongs to and get the community measures
  x <- membership(community_gn)
  com_measures <- get_com_measures(x, .y)

  # Create a list that will be be the output
  data.frame(
    ego_id = .y,
    len_comm_gn = comm_len, 
    modularity_gn = modularity_score,
    n_sics_gn = com_measures[1],
    n_sics_daily_gn = com_measures[2],
    n_sics_work_gn = com_measures[3])

})

# For those where modularity is 25, set values to NA to exclude from analyses
comm_gn_df <- comm_gn_df %>%
  mutate(across(n_sics_gn:n_sics_work_gn, ~ifelse(modularity_gn == 25, NA, .x)))


# Infomap algorithm for community detection
comm_infomap_df <- map2_dfr(graphs, ego_ids, ~ {
  community_infomap = cluster_infomap(
    .x,
    e.weights = NA,
    v.weights = NA,
    nb.trials = 50,
    modularity = TRUE)

  # Displays one value for modularity
  modularity_score <- modularity(community_infomap)

  # The number of communities
  comm_len <- length(community_infomap)

  # Create a list that will be be the output
  data.frame(
    ego_id = .y, 
    len_comm_infomap = comm_len, 
    modularity_infomap = modularity_score)

  # Lists what community each alter belongs to and get the community measures
  x <- membership(community_infomap)
  com_measures <- get_com_measures(x, .y)

  # Create a list that will be be the output
  data.frame(
    ego_id = .y,
    len_comm_im = comm_len, 
    modularity_im = modularity_score,
    n_sics_im = com_measures[1],
    n_sics_daily_im = com_measures[2],
    n_sics_work_im = com_measures[3])
})

# For those where modularity is 25, set values to NA to exclude from analyses
comm_infomap_df <- comm_infomap_df %>%
  mutate(across(n_sics_im:n_sics_work_im, ~ifelse(modularity_im == 25, NA, .x)))
  

 
# Display summary table -------------------------------------------------------
# Create name modifying function to stack data sets and summarise data
modify_names <- function(df) {
  names(df) <- c("ego_id", "len_comm", "modularity", "n_sics", "n_sics_daily", "n_sics_work")

  return(df)
}

# Summarise values. Output from the table is to be placed in an excel file for sharing
# with Jun, Chris, and Allison
comm_summary_tab <- bind_rows(
  comm_lu_df %>% modify_names() %>% mutate(type = "louvain"),
  comm_gn_df %>% modify_names() %>% mutate(type = "girvan-newman"),
  comm_infomap_df %>% modify_names() %>% mutate(type = "infomap")) %>%
  select(-ego_id) %>%
  tbl_summary(
    by = type,
    type = everything() ~ "continuous",
    statistic = all_continuous() ~ "{min}, {max}; {mean} ({sd})"
    )


# <!--///////////////////// PUT DATA SETS TOGETHER FOR QA ////////////////////
 # These are node level betweenness degree and closenes along with the community
# detection algorithms.
output_df <- reduce(
  list(btw_centr_df, degree_df, closeness_df, comm_lu_df, comm_gn_df, comm_infomap_df),
  left_join, by = "ego_id"
)

# Merge the output df with the alter tie columns for QA
output_df <- output_df %>%
  left_join(filtered_ego_data %>% select(ego_id, alter1:alter25, m1_2:m24_25), by = "ego_id")

# Since the alter ties were binarized, create binary version.
output_df <- output_df %>%
  mutate(across(m1_2:m24_25, ~ ifelse(.x == "Not at all likely", "No", "Yes")))

# Count the number of unique values in the m* columns to see who ends up with
# no variation in responses responses for the m* columns
output_df %>%
  left_join(
    output_df %>%
      select(ego_id, m1_2:m24_25) %>%
      pivot_longer(
        cols = m1_2:m24_25,
        names_to = "name",
        values_to = "value") %>%
      group_by(ego_id) %>%
      summarise(unique_m_vals = n_distinct(value), .groups = "drop") %>%
      mutate(straightlined_m_cols = ifelse(unique_m_vals == 1, 1,0)), 
    by = "ego_id") %>%
  select(ego_id, unique_m_vals, straightlined_m_cols, everything()) %>%
  write_csv(., "C:\\Users\\rodrica2\\OneDrive - The University of Colorado Denver\\Documents\\DFM\\projects\\hsq\\scripts\\network_measures\\data\\qa\\hsq_pna_network_node_metrics.csv")


# ////////////////////////// homophily gender ////////////////////////// 
comp_ei_results_gender <- comp_ei(
  final_ego_network, 
  ego.attr = "gender", 
  alt.attr = "altgender_a") %>%
  rename(homophily_gender = ei)


# ///////////////////////// homophily age_cat ////////////////////////// 
comp_ei_results_age_cat <- comp_ei(
  final_ego_network, 
  ego.attr = "age_cat", 
  alt.attr = "altage_a") %>%
  rename(homophily_age_cat = ei)


# /////////////////// collect global network level metrics ////////////////////
# Stitch together network results
# Commented out
network_metrics <- reduce(
  list(
    density_results,
    components_results,
    btw_centr_results,
    deg_centr_results,
    comp_ei_results_gender,
    comp_ei_results_age_cat), 
  left_join, by = ".egoID") %>%
mutate(ego_id = .egoID)


# /////////////// Percent of alters supporting quitting //////////////// 
percent_support_quit <- 
  data %>%
  select(ego_id, arm, starts_with("altsupport_a")) %>%
  mutate(across(starts_with("altsupport_a"), ~ ifelse(.x == "Yes", 1, 0))) %>%
  mutate(row_sum = rowSums(select(., starts_with("altsupport_a")))) %>%
  mutate(perc_supp_quit = (row_sum / 25)) %>%
  select(ego_id, perc_supp_quit)
  
# Merge in percent of alters supporting quitting
data <- data %>%
  left_join(
    percent_support_quit,
    by = "ego_id")

# Create an event name and record id for merging
data <- data %>%
  mutate(event_name = sub("^[^_]*_", "", ego_id),
         record_id = sub("_.*", "", ego_id))


# //////////////// Write out collected dat to .csv file ////////////////
# At this point the node level measures like node centrality, closeness, and 
# degree have not been arranged to a point for output, because of issues with 
# ties. There are cases where there are as many ties as there are alters. 
# Usually, these result from cases where each alter has a value of 0 for a 
# given metric. Suggests possible data quality issues
data %>%
  left_join(
    network_metrics,
    by = "ego_id") %>%
  select(hsqid, arm, event_name, perc_supp_quit:homophily_age_cat, - .egoID) %>%
  arrange(hsqid) %>%
  write_csv(here("analyses_sas", "data", "hsq_network_data.csv"), na = "")
