Splitting reports allows users to create reports specific to 
particular subsets of respondents, namely those with matching
responses in a particular column of response data. This can be used
for studying the breakdown of responses across particular questions,
surveyed demographics information, or other meta-data included
in the Qualtrics response data. 

The `make_split_results_tables`, `make_split_text_appendices`, 
and `make_split_coded_comments` are all available for use 
to create reports split into distinct respondent groups by their 
responses to a specified response column or columns. 

## Using `make_split_results_tables`

```R
# The single function call works. However, it will output 
# to a temporary file and may not be named what you'd like.
make_split_results_tables(split_by='column_name')

# The make_split_* functions can take either a single 
# column name or a list of them.
make_split_results_tables(split_by=c('column_name_1', 'column_name_2'))

# The function has many parameters which you may use. 
make_split_results_tables(
  qsf_path = "C:/Example/Path/to/SurveyFile.qsf",
  csv_path = "C:/Example/Path/to/ResponseFile.csv",
  headerrows = 3,
  output_dir = "C:/Example/Path/Output Testing/",
  split_by = "column_name",
  n_threshold = 15)
```

## Without Using `make_split_results_tables`

```R
# Use get_setup or another method to import survey data into R.
get_setup()

# After importing survey data, use split_respondents to split 
# the blocks containing the survey's questions and responses into
# a list of split_blocks where each block contains respondents
# which all agree in their response in the "column_name" response
# column in the responses data. If 'already_loaded=TRUE' is 
# passed as a parameter, the split_respondents function 
# automatically pulls in data from the global environment such as
# survey, blocks, respondenses, questions, and more. Otherwise,
# those arguments can also be passed directly. Replace "column_name" 
# with the name of the column you would like to split the respondents across. 
split_blocks <- split_respondents(response_column = "column_name", 
                                  already_loaded = TRUE)

# The output_dir can be passed directly as a string representative
# of the path, or the choose.dir() method works on Windows to 
# use an interactive dialog box to choose a directory.
output_dir <- choose.dir()

# Since there are multiple survey structures in the list of 
# split_blocks, each containing a list of blocks with questions 
# and split responses inserted into them, to generate reports 
# we want to reports for each split_block within the split_blocks.
for (i in 1:length(split_blocks)) {
  split_block <- split_blocks[[i]]
  
  # generating split results tables
  html_2_pandoc(html = c(blocks_header_to_html(split_block), 
                         tabelize_blocks(split_block)), 
                file_name = paste0('results tables ', i, '.docx'),
                output_dir = output_dir)
  
  # generating split text appendices
  html_2_pandoc(html = c(blocks_header_to_html(split_block),
                         text_appendices_table(blocks, 
                                               original_first_rows,
                                               flow)),
                file_name = paste0('text appendices ', i, '.docx'),
                output_dir = output_dir)
}
```

## Use with Multiple Columns

Splitting respondents into distinct categories 
of responses across multiple response columns is done 
by merging the relevant response columns together and 
splitting across the distinct levels in the constructed 
merged response column. 

```R
# We construct the merged response column using the 
create_merged_response_column method. We provide
# to it a list of columns to entry-wise concatenate 
# and a name for the column to be appended to our 
# responses dataframe. 
responses <- create_merged_response_column(
    response_columns = c('column1', 'column2'),
    col_name = 'new_column_name')

# Next, we split across that column just as in the basic example. 
split_blocks <-  split_respondents(response_column = "new_column_name", 
                                   already_loaded = TRUE)

# Finally, we choose a directory and generate reports.
output_dir <- choose.dir()
for (i in 1:length(split_blocks)) {
  split_block <- split_blocks[[i]]
  html_2_pandoc(html = c(blocks_header_to_html(split_block), 
                         tabelize_blocks(split_block)), 
                file_name = paste0('results tables ', i, '.docx'),
                output_dir = output_dir)
}
```

![Example Code Usage](http://i.imgur.com/mCoMBX9.png)
![Example Output](http://i.imgur.com/k6g36nq.png)
