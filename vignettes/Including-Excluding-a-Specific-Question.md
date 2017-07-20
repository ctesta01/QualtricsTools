## qtSkip
If you would like to generate a report with certain questions excluded, the following 
code snippet produces results tables and text appendices without question one:

```R
# Load your survey,
# Or use get_setup(already_loaded=TRUE) in an empty environment to load a sample survey.
get_setup()

# We can use find_question_index to find the index of a question 
# in the questions list, or we could reference questions[[7]][['qtSkip']] directly
# to tag it for skipping. We could also look up the index of a 
# question by using its QID through find_question_index_by_qid(questions, "QID1").
questions[[find_question_index(questions, "Q1")]][['qtSkip']] <- TRUE

# We have to update the blocks, since the blocks are the structure everything is 
# consolidated into before rendering results tables and text appendices.
blocks <- questions_into_blocks(questions, blocks)

# Output without QID1
make_results_tables(output_dir='C:/Example/Path/Here/')
make_text_appendices(output_dir='C:/Example/Path/Here/')
```

## verbatimSkip

Or, for example, if you want to only exclude the text entry component 
of a question, the following code sample shows how to use the 
`verbatimSkip` tag to exclude a text entry question. 
```R
# We load a survey. 
get_setup()

# We can see which questions are text entry questions in the survey as follows.
which(sapply(questions, is_text_entry))
# [1] 1

# We will skip the one text entry question in this survey, so that when 
# we make the text appendices they are empty. 
questions[[1]][['verbatimSkip']] = TRUE
blocks <- questions_into_blocks(questions, blocks)

# This output won't have any text entry questions.
make_text_appendices(output_dir='C:/Example/Path/Here/')
```

For example, a potential application of verbatimSkip could be generating reports
of coded comments without any verbatim comments text appendices. 
```R
get_setup()
for (i in 1:length(questions)) questions[[i]][['verbatimSkip']] <- TRUE
blocks <- questions_into_blocks(questions, blocks)
make_coded_comments(
  sheets_dir = "C:/Example/Path/to/Coded/Comment/Sheets/", 
  output_dir = "C:/Example/Path/")
```