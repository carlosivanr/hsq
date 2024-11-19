# #############################################################################
# Carlos Rodriguez, PhD. CU Anschutz Detpt. of Family Medicine
# 11-15-2024
# HSQ - import and prep NJH data
#
# Description:
# The purpose of this script is import the NJH data extracts. The NJH data
# extract is a file that combines the state specific quitline intake questions.
# 
# A secondary purpose of this script is to rename the NJH extract columns.
# Each state may have a different set of intake questions and therefore has a
# unique codebook to refer to the column names. The column names are ambiguous
# because they are named as SI 1, SI5a, or OI 7b as examples. In order to make
# these columns human readable, this script will import state specific code
# books to determine the intake questions that correspond to the ambiguous
# column names 
# #############################################################################
 
# Import libraries ------------------------------------------------------------
import pandas as pd
import os

# Read in the data extract ---------------------------------------------------
proj_root = 'C:\\Users\\rodrica2\\OneDrive - The University of Colorado Denver\\Documents\\DFM\\projects\\hsq'

sub_dir = '\\scripts\\njh_data\\codebooks'

# data_dir = '\\scripts\\njh_data\\Helpers Quit Study Data Extract 10.10.24.xlsx'
data_dir = '\\scripts\\njh_data\\Helpers Quit Study Data Extract 11.13.24.xlsx'

file_path = proj_root + data_dir

data = pd.read_excel(file_path, header = 1)


# Get column names ------------------------------------------------------------
# Get the names from the data extract of the columns that need question labels
# Columns that need to be determined are AI1 through UT 12

# This is the column range for an older version of the data extract
# 10-10-2024
# col_names = pd.DataFrame({'MDS Question Id': data.loc[:, 'AI 1':'UT 12'].columns,
#                           'Question Text': pd.NA})

# 11-13-2024
col_names = pd.DataFrame({'MDS Question Id': data.loc[:, 'NJ 31':'UT 9'].columns,
                          'Question Text': pd.NA})


# Import the code book for the data extract -----------------------------------
# Each state has a different code book, therefore list all of the .xlsx files
# to loop through each file and merge in the required column names
files = pd.DataFrame({'file': os.listdir(proj_root + sub_dir)})

# Create the state column
files['state'] = files['file'].str.slice(0,2)

# for each file in files
# read in the .xlsx file
# skip the first 9 rows
# remove columns that start with '...'
# Only fill in the values of the column
# names that remain to be answered

# Outputs to a data frame named col_names
for x in files['file']:
  print(x)
  path = proj_root + sub_dir + '\\'
  temp = pd.read_excel(path + x, header = 9)

  to_be_filled = col_names[col_names['Question Text'].isna()][['MDS Question Id']]

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

    col_names = pd.concat([output, 
                          col_names[~col_names['MDS Question Id']
                                    .isin(output['MDS Question Id'])]], 
                          ignore_index=True)


# Reformat the primary data frame ---------------------------------------------
# Replace the acronym names with human radable labels

# Sort col_names to match the order in which they appear in the primary df
col_names = col_names.sort_values(by = 'MDS Question Id')

# Modify the question text for those with a lot of text and/or contains html
col_names.loc[col_names["MDS Question Id"] == 'SI 1', 'Question Text'] = "How may I help you today"
col_names.loc[col_names["MDS Question Id"] == 'SI 18e', 'Question Text'] = "Are you American Indian or Alaska Native?"
col_names.loc[col_names["MDS Question Id"] == 'OI 24', 'Question Text'] = "Do you consider yourself to be gay, lesbian or bisexual?"


# Create a dictionary for renaming columns
mapper = dict(zip(col_names["MDS Question Id"], col_names["Question Text"]))

# Create a new data frame with labeled columns by applying the mapper to the
# column names
data_labeled = data.rename(columns = mapper)

# Output data to a .csv -------------------------------------------------------
output_dir = '\\scripts\\njh_data\\'

# Output the unlabeled data
data_out = proj_root + output_dir + 'njh_data.csv'
data.to_csv(data_out, index=False)

# Output the labels for the data
mapper = pd.DataFrame.from_dict(mapper, orient = 'index')
mapper.index.name = 'question'
mapper = mapper.rename(columns = {0: 'label'})
mapper = mapper.reset_index()

mapper_out = proj_root + output_dir + 'mapper.csv'
mapper.to_csv(mapper_out)