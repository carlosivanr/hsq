# #############################################################################
# Carlos Rodriguez, PhD. CU Anschutz Detpt. of Family Medicine
# 11-15-2024
# HSQ - import and prep NJH data
#
# Description:
# The purpose of this script is import the NJH data extract. The NJH data
# extract is a file that combines the state specific quitline intake questions.
# 
# A secondary purpose of this script is to rename the NJH extract columns.
# Each state may have a different set of intake questions and therefore has a
# unique codebook to refer to the column names. The column names are ambiguous
# because they are named as SI 1, SI5a, or OI 7b as examples. In order to make
# these columns human readable, this script will import state specific code
# books to determine the intake questions that correspond to the ambiguous
# column names.

# TO-DO:
# 1. Determine which states offered the default, american india, and youth 
# versions of each question. Determine if SI 18e is the question that branches
# the american indian version to determine the denominators. Reached out to 
# Amanda Proctor at NJH on 7/31/2025.
# 2. Some questions are asked on intake while others are asked on eligibility
# Determine which question comes from what codebook.
# #############################################################################
 
# Import libraries ------------------------------------------------------------
import pandas as pd
import numpy as np
import os

# Read in the NJH data --------------------------------------------------------
proj_root = 'C:\\Users\\rodrica2\\OneDrive - The University of Colorado Denver\\Documents\\DFM\\projects\\hsq'

sub_dir = '\\scripts\\njh_data\\codebooks'

data_dir = '\\scripts\\njh_data\\Helpers Quit Study Data Extract 01.06.25.xlsx'

file_path = proj_root + data_dir

data = pd.read_excel(file_path, header = 1)

# Prepare data ----------------------------------------------------------------
# REPLACE DOUBLE SPACES WITH ONE SPACE IN THE COLUMN NAMES like SI 7d and SI 8b
col_names = data.columns.tolist()

# Replace double spaces with single space in column names
col_names = [col.replace('  ', ' ') for col in col_names]

# Assign modified col_names as the column names
data.columns = col_names

# CALCULATE DAYS SINCE LAST X CIGARETTE, CIGAR, ETC.
# Create IntakeDate by filling NAs from WebIntke to PhoneIntake since 
# PhoneIntake is the more complete column
data['IntakeDate'] = data['PhoneIntakeDate'].fillna(data['WebIntakeDate'])

# Convert to datetime and format
data['IntakeDate'] = pd.to_datetime(data['IntakeDate'])

# 1. Last date cigarette
data['Last Cigarette Date (SI 8a)'] = pd.to_datetime(data['Last Cigarette Date (SI 8a)'])
data['days_last_cigarette'] = (data['IntakeDate'] - data['Last Cigarette Date (SI 8a)']).dt.days

# 2. Last date cigar
data['Last Cigar Date (SI 8b)'] = pd.to_datetime(data['Last Cigar Date (SI 8b)'])
data['days_last_cigar'] = (data['IntakeDate'] - data['Last Cigar Date (SI 8b)']).dt.days

# 3. Last date pipe
data['Last Pipe Date (SI 8c)'] = pd.to_datetime(data['Last Pipe Date (SI 8c)'])
data['days_last_pipe'] = (data['IntakeDate'] - data['Last Pipe Date (SI 8c)']).dt.days


# 4. Last date chew
data['Last SLT Date (SI 8d)'] = pd.to_datetime(data['Last SLT Date (SI 8d)'])
data['days_last_SLT'] = (data['IntakeDate'] - data['Last SLT Date (SI 8d)']).dt.days


# 5. Last date other tobacco
data['Last Other Tobacco Date (SI 8e)'] = pd.to_datetime(data['Last Other Tobacco Date (SI 8e)'])
data['days_last_other_tbco'] = (data['IntakeDate'] - data['Last Other Tobacco Date (SI 8e)']).dt.days


# MERGE OHIO DATA -------------------------------------------------------------
# * May need to add Ohio data here and not in the njh_data_eda.qmd file. Will 
# depend on if OH is included in newest data delivery or not.


# Get question ids ------------------------------------------------------------
# Capture the question ids of NJ 31 through UT 9 and turn them into a dataframe
# where the the text is set to NA. These are the question ids for which the 
# question text needs to be determined. The question text will be found in the 
# codebooks, where there are two codebooks (intake and eligibility) for every 
# state.
question_id_map = pd.DataFrame({'MDS Question Id': data.loc[:, 'NJ 31':'UT 9'].columns,
                                'Question Text': pd.NA})


# Import the code book for the data extract -----------------------------------
# Each state has a different code book, therefore list all of the .xlsx files
# to loop through each file and merge in the required question text for each
# question id
files = pd.DataFrame({'file': os.listdir(proj_root + sub_dir)})

# Create the state column by slicing the string for the first two letters
files['state'] = files['file'].str.slice(0,2)


# Create an empty data frame that will represent a map of which questions were
# asked by a given state.
state_question_map = pd.DataFrame()

for x in files['file']:
  # This for-loop is designed to read in each state's .xlsx codebook. The first
  # 9 rows are skipped because they are formatted header rows, and do not 
  # contain data of interest. For each codebook, the questions in question_id_map
  # are cross referenced and only the questions in question_id_map that need a label
  # are scanned, pulled out and then the question text is merged into question_id_map
  print(x)
  # Set the path root
  path = proj_root + sub_dir + '\\'

  # Read in the file using path as temp
  temp = pd.read_excel(path + x, header = 9)

  # Capture the MDS Question Ids, add the state, then append to the question 
  # map
  questions = temp[['Attribute', 'MDS Question Id']].dropna()
  state = files[files['file'] == x]['state'].iloc[0]
  questions['state'] = state
  state_question_map = pd.concat([state_question_map, questions], ignore_index = True)

  # Get a list of column names that need to have the question text
  to_be_filled = question_id_map[question_id_map['Question Text'].isna()][['MDS Question Id']]

  if to_be_filled.shape[0] > 0:
    # Filter the temp data frame to only the rows that need to be assigned a Question Text
    # Then group by MDS Question Id and select one row, because the same MDS Id may have
    # slightly different question text. Then reset the index to place the MDS Id column back
    # into the data frame, and then finally select only the two columns of interest
    col_labels = (temp[temp['MDS Question Id']
    .isin(to_be_filled['MDS Question Id'])]
    .groupby('MDS Question Id')
    .first().reset_index()
    [['MDS Question Id', 'Question Text']]
    )

    output = (to_be_filled
              .merge(col_labels, how = 'left', on = 'MDS Question Id')
    )

    output = output.dropna()

    question_id_map = pd.concat([output, 
                          question_id_map[~question_id_map['MDS Question Id']
                                    .isin(output['MDS Question Id'])]], 
                          ignore_index=True)


# question_id_map represents a cross walk from the question id such as NJ31 to 
# its corresponding question text. Although each state asks a slightly 
# different set of question ids, all question ids of interest are found in the
# question_id_map data frame which are derived from the data extract. So the 
# state question map should be filtered to the rows where MDS Quesiont Id is 
# in question_id_map
state_question_map = (
  state_question_map[state_question_map["MDS Question Id"].isin(question_id_map["MDS Question Id"])]
  )

# Why do some counts have over 100?
(state_question_map
  .groupby("state")
  .size())

az_questions = state_question_map[state_question_map['state'] == "AZ"]

# * Some are in duplicates and some are in triplicates
# Some states, AZ UT 9 for example, are asked 3x according to the intake 
# codebook.
# Row 435 - Default
# Row 788 - American Indian
# Row 1226 - Youth

# Check that each state does in fact have a different number of questions
state_counts = (state_question_map
  .drop_duplicates()
  .groupby("state")['MDS Question Id']
  .count())

state_question_map["Attribute"] = np.where(
  state_question_map["Attribute"].str.contains("American"), "American Indian",
  np.where(
    state_question_map["Attribute"].str.contains("Yout"), "Youth",
    "Default"
  )
)

# * Massachusetts MA asks two additional questions out of the current list of states
# as of 07/28/2025
state_question_map['asked'] = 1  # Create a column of 1s

state_question_map = state_question_map.drop_duplicates()

# Pivot wider with one row per MDS Question Id, and a column per state
result = state_question_map.pivot_table(index=['state', 'MDS Question Id'], 
                        columns='Attribute', 
                        values='asked', 
                        aggfunc='max', 
                        fill_value=0).reset_index()

# The questions that MA asks that are additional ar OI 7b and OI 7f, which asks
# if cigars or vaping in the parent question are menthol

# Save the state_question_map to .csv
output_dir = '\\scripts\\njh_data\\'

# Output the unlabeled data
data_out = proj_root + output_dir + 'state_question_map.csv'
state_question_map.to_csv(data_out, index=False)

state_question_map.groupby('MDS Question Id')['state'].nunique().reset_index(name='n_unique')

# Reformat the primary data frame ---------------------------------------------
# Replace the acronym names with human radable labels

# Sort question_id_map to match the order in which they appear in the primary df
question_id_map = question_id_map.sort_values(by = 'MDS Question Id')

# Modify the question text for those with a lot of text and/or contains html
question_id_map.loc[question_id_map["MDS Question Id"] == 'SI 1', 'Question Text'] = "How may I help you today"
question_id_map.loc[question_id_map["MDS Question Id"] == 'SI 18e', 'Question Text'] = "Are you American Indian or Alaska Native?"
question_id_map.loc[question_id_map["MDS Question Id"] == 'OI 24', 'Question Text'] = "Do you consider yourself to be gay, lesbian or bisexual?"


# Create a dictionary for renaming columns
mapper = dict(zip(question_id_map["MDS Question Id"], question_id_map["Question Text"]))

# Create a new data frame with labeled columns by applying the mapper to the
# column names
data_labeled = data.rename(columns = mapper)





# Output data to a .csv -------------------------------------------------------
# Output the unlabeled data
data_out = proj_root + output_dir + 'njh_data.csv'
data.to_csv(data_out, index=False)

# Output the unlabeled data to the analyses folder
data.to_csv(
"C:\\Users\\rodrica2\OneDrive - The University of Colorado Denver\\Documents\\DFM\\projects\\hsq\\analyses_sas\\data",
index = False)

# Output the labels for the data
mapper = pd.DataFrame.from_dict(mapper, orient = 'index')
mapper.index.name = 'question'
mapper = mapper.rename(columns = {0: 'label'})
mapper = mapper.reset_index()

mapper_out = proj_root + output_dir + 'text_question_map.csv'
mapper.to_csv(mapper_out)