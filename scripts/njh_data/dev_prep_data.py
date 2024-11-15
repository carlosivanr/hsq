import pandas as pd
import os

# Read in the data extract
proj_root = 'C:\\Users\\rodrica2\\OneDrive - The University of Colorado Denver\\Documents\\DFM\\projects\\hsq'

sub_dir = '\\scripts\\njh_data\\codebooks'

data_dir = '\\scripts\\njh_data\\Helpers Quit Study Data Extract 10.10.24.xlsx'

file_path = proj_root + data_dir

data = pd.read_excel(file_path, header = 1)

# Get the names from the data extract of the columns that need question labels
# Columns that need to be determined are AI1 through UT 12
col_names = pd.DataFrame({'MDS Question Id': data.loc[:, 'AI 1':'UT 12'].columns,
                          'Question Text': pd.NA})


# Import the code book for the data extract
# Each state has a different code book
files = pd.DataFrame({'file': os.listdir(proj_root + sub_dir)})

# Create the state column
files['state'] = files['file'].str.slice(0,2)

# for each file in files
# read in the .xlsx file
# skip the first 9 rows
# remove columns that start with '...'
# Only fill in the values of the column
# names that remain to be answerd

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

