# Test mc_single_answer_results
context("Testing matrix_multiple_answer_results")

# Load the original_first_rows from the Long Exhaustive Sample Survey.
# This is needed for the next several tests that use questions from the
# Long Exhaustive Sample Survey.
original_first_rows <- readRDS(
  file.path(
    find.package('QualtricsTools'),
    'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
    'original_first_rows.rds'
  )
)

# Long Exhaustive Sample Survey: Q29
test_that(
  "Test that mc_multiple_answer_results is correct for Q29 in the Long Exhaustive Sample Survey",
  {
    # Load the question, without the results tables, for processing.
    Q29 <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q29.rds'
      )
    )

    # Process the question and insert results tables into it.
    Q29 <- process_question_results(Q29, original_first_rows)

    # Load the previously computed results table.
    Q29_results_table <-  readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q29_results_table.rds'
      )
    )
    Q29_results_table <- as.data.frame(Q29_results_table)

    # Check that the previously computed results and new results match.
    expect_true(all(Q29[['Table']] == Q29_results_table) &&
                  all(names(Q29[['Table']]) == names(Q29_results_table)))
  }
)



# Long Exhaustive Sample Survey: Q36
test_that(
  "Test that mc_multiple_answer_results is correct for Q36 in the Long Exhaustive Sample Survey",
  {
    # Load the question, without the results tables, for processing.
    Q36 <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q36.rds'
      )
    )

    # Process the question and insert results tables into it.
    Q36 <- process_question_results(Q36, original_first_rows)

    # Load the previously computed results table.
    Q36_results_table <-  readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q36_results_table.rds'
      )
    )
    Q36_results_table <- as.data.frame(Q36_results_table)

    # Check that the previously computed results and new results match.
    expect_true(all(Q36[['Table']] == Q36_results_table) &&
                  all(names(Q36[['Table']]) == names(Q36_results_table)))
  }
)



# Long Exhaustive Sample Survey: Q37
test_that(
  "Test that mc_multiple_answer_results is correct for Q37 in the Long Exhaustive Sample Survey",
  {
    # Load the question, without the results tables, for processing.
    Q37 <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q37.rds'
      )
    )

    # Process the question and insert results tables into it.
    Q37 <- process_question_results(Q37, original_first_rows)

    # Load the previously computed results table.
    Q37_results_table <-  readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q37_results_table.rds'
      )
    )
    Q37_results_table <- as.data.frame(Q37_results_table)

    # Check that the previously computed results and new results match.
    expect_true(all(Q37[['Table']] == Q37_results_table) &&
                  all(names(Q37[['Table']]) == names(Q37_results_table)))
  }
)


# Long Exhaustive Sample Survey: q30_cooking_style
test_that(
  "Test that mc_multiple_answer_results is correct for q30_cooking_style in the Long Exhaustive Sample Survey",
  {
    # Load the question, without the results tables, for processing.
    q30_cooking_style <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'q30_cooking_style.rds'
      )
    )

    # Process the question and insert results tables into it.
    q30_cooking_style <- process_question_results(q30_cooking_style, original_first_rows)

    # Load the previously computed results table.
    q30_cooking_style_results_table <-  readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'q30_cooking_style_results_table.rds'
      )
    )
    q30_cooking_style_results_table <- as.data.frame(q30_cooking_style_results_table)

    # Check that the previously computed results and new results match.
    expect_true(all(q30_cooking_style[['Table']] == q30_cooking_style_results_table) &&
                  all(names(q30_cooking_style[['Table']]) == names(q30_cooking_style_results_table)))
  }
)


# Long Exhaustive Sample Survey: Q31
test_that(
  "Test that mc_multiple_answer_results is correct for Q31 in the Long Exhaustive Sample Survey",
  {
    # Load the question, without the results tables, for processing.
    Q31 <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q31.rds'
      )
    )

    # Process the question and insert results tables into it.
    Q31 <- process_question_results(Q31, original_first_rows)

    # Load the previously computed results table.
    Q31_results_table <-  readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q31_results_table.rds'
      )
    )
    Q31_results_table <- as.data.frame(Q31_results_table)

    # Check that the previously computed results and new results match.
    expect_true(all(Q31[['Table']] == Q31_results_table) &&
                  all(names(Q31[['Table']]) == names(Q31_results_table)))
  }
)


# Long Exhaustive Sample Survey: Q32
test_that(
  "Test that mc_multiple_answer_results is correct for Q32 in the Long Exhaustive Sample Survey",
  {
    # Load the question, without the results tables, for processing.
    Q32 <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q32.rds'
      )
    )

    # Process the question and insert results tables into it.
    Q32 <- process_question_results(Q32, original_first_rows)

    # Load the previously computed results table.
    Q32_results_table <-  readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q32_results_table.rds'
      )
    )
    Q32_results_table <- as.data.frame(Q32_results_table)

    # Check that the previously computed results and new results match.
    expect_true(all(Q32[['Table']] == Q32_results_table) &&
                  all(names(Q32[['Table']]) == names(Q32_results_table)))
  }
)



# Long Exhaustive Sample Survey: Q33
test_that(
  "Test that mc_multiple_answer_results is correct for Q33 in the Long Exhaustive Sample Survey",
  {
    # Load the question, without the results tables, for processing.
    Q33 <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q33.rds'
      )
    )

    # Process the question and insert results tables into it.
    Q33 <- process_question_results(Q33, original_first_rows)

    # Load the previously computed results table.
    Q33_results_table <-  readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q33_results_table.rds'
      )
    )
    Q33_results_table <- as.data.frame(Q33_results_table)

    # Check that the previously computed results and new results match.
    expect_true(all(Q33[['Table']] == Q33_results_table) &&
                  all(names(Q33[['Table']]) == names(Q33_results_table)))
  }
)




# Long Exhaustive Sample Survey: Q34
test_that(
  "Test that mc_multiple_answer_results is correct for Q34 in the Long Exhaustive Sample Survey",
  {
    # Load the question, without the results tables, for processing.
    Q34 <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q34.rds'
      )
    )

    # Process the question and insert results tables into it.
    Q34 <- process_question_results(Q34, original_first_rows)

    # Load the previously computed results table.
    Q34_results_table <-  readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q34_results_table.rds'
      )
    )
    Q34_results_table <- as.data.frame(Q34_results_table)

    # Check that the previously computed results and new results match.
    expect_true(all(Q34[['Table']] == Q34_results_table) &&
                  all(names(Q34[['Table']]) == names(Q34_results_table)))
  }
)

# Long Exhaustive Sample Survey: Q35
test_that(
  "Test that mc_multiple_answer_results is correct for Q35 in the Long Exhaustive Sample Survey",
  {
    # Load the question, without the results tables, for processing.
    Q35 <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q35.rds'
      )
    )

    # Process the question and insert results tables into it.
    Q35 <- process_question_results(Q35, original_first_rows)

    # Load the previously computed results table.
    Q35_results_table <-  readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q35_results_table.rds'
      )
    )
    Q35_results_table <- as.data.frame(Q35_results_table)

    # Check that the previously computed results and new results match.
    expect_true(all(Q35[['Table']] == Q35_results_table) &&
                  all(names(Q35[['Table']]) == names(Q35_results_table)))
  }
)

