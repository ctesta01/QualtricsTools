If you have imported a survey in R and would like to poke around the results tables, they are stored in each question's `[['Table']]` element. The questions are automatically processed when loading data using `get_setup`, so we can investigate question results like so:

```R
# Interactively choose survey data
get_setup()

# Take a look at Question 1's results table
questions[[1]][['Table']]

# Find a question by its Data Export Tag and 
# take a look at its results
i = find_question_index(exporttag = "q2_favorite")
questions[[i]][['Table']]

# The response data is also contained in a question,
# and you may access it through 
questions[[i]][['Responses']]

# After updating a question or its responses in R, use 
# process_question_results to update the results tables
questions[[i]] <- process_question_results(questions[[i]])

# If you want to process and take a look 
# at the results tables immediately, you should 
# run something like 
process_question_results(questions[[i]])[['Table']]
```

![Example of process_question_results](http://i.imgur.com/NznhtDu.png)