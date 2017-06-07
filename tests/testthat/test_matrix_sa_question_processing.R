# Test mc_single_answer_results
context("Testing matrix_single_answer_results")

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

# Long Exhaustive Sample Survey: Q8
test_that(
  "Test that mc_multiple_answer_results is correct for Q8 in the Long Exhaustive Sample Survey",
  {
    # Load the question, without the results tables, for processing.
    Q8 <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q8.rds'
      )
    )

    # Process the question and insert results tables into it.
    Q8 <- process_question_results(Q8, original_first_rows)

    # Load the previously computed results table.
    Q8_results_table <-  readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q8_results_table.rds'
      )
    )
    Q8_results_table <- as.data.frame(Q8_results_table)

    # Check that the previously computed results and new results match.
    expect_true(all(Q8[['Table']] == Q8_results_table) &&
                  all(names(Q8[['Table']]) == names(Q8_results_table)))
  }
)


# Long Exhaustive Sample Survey: Q27
test_that(
  "Test that mc_multiple_answer_results is correct for Q27 in the Long Exhaustive Sample Survey",
  {
    # Load the question, without the results tables, for processing.
    Q27 <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q27.rds'
      )
    )

    # Process the question and insert results tables into it.
    Q27 <- process_question_results(Q27, original_first_rows)

    # Load the previously computed results table.
    Q27_results_table <-  readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q27_results_table.rds'
      )
    )
    Q27_results_table <- as.data.frame(Q27_results_table)

    # Check that the previously computed results and new results match.
    expect_true(all(Q27[['Table']] == Q27_results_table) &&
                  all(names(Q27[['Table']]) == names(Q27_results_table)))
  }
)

# Long Exhaustive Sample Survey: Q23
test_that(
  "Test that mc_multiple_answer_results is correct for Q23 in the Long Exhaustive Sample Survey",
  {
    # Load the question, without the results tables, for processing.
    Q23 <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q23.rds'
      )
    )

    # Process the question and insert results tables into it.
    Q23 <- process_question_results(Q23, original_first_rows)

    # Load the previously computed results table.
    Q23_results_table <-  readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q23_results_table.rds'
      )
    )
    Q23_results_table <- as.data.frame(Q23_results_table)

    # Check that the previously computed results and new results match.
    expect_true(all(Q23[['Table']] == Q23_results_table) &&
                  all(names(Q23[['Table']]) == names(Q23_results_table)))
  }
)


# Long Exhaustive Sample Survey: Q24
test_that(
  "Test that mc_multiple_answer_results is correct for Q24 in the Long Exhaustive Sample Survey",
  {
    # Load the question, without the results tables, for processing.
    Q24 <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q24.rds'
      )
    )

    # Process the question and insert results tables into it.
    Q24 <- process_question_results(Q24, original_first_rows)

    # Load the previously computed results table.
    Q24_results_table <-  readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q24_results_table.rds'
      )
    )
    Q24_results_table <- as.data.frame(Q24_results_table)

    # Check that the previously computed results and new results match.
    expect_true(all(Q24[['Table']] == Q24_results_table) &&
                  all(names(Q24[['Table']]) == names(Q24_results_table)))
  }
)


# Long Exhaustive Sample Survey: Q25
test_that(
  "Test that mc_multiple_answer_results is correct for Q25 in the Long Exhaustive Sample Survey",
  {
    # Load the question, without the results tables, for processing.
    Q25 <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q25.rds'
      )
    )

    # Process the question and insert results tables into it.
    Q25 <- process_question_results(Q25, original_first_rows)

    # Load the previously computed results table.
    Q25_results_table <-  readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q25_results_table.rds'
      )
    )
    Q25_results_table <- as.data.frame(Q25_results_table)

    # Check that the previously computed results and new results match.
    expect_true(all(Q25[['Table']] == Q25_results_table) &&
                  all(names(Q25[['Table']]) == names(Q25_results_table)))
  }
)


# Long Exhaustive Sample Survey: Q26
test_that(
  "Test that mc_multiple_answer_results is correct for Q26 in the Long Exhaustive Sample Survey",
  {
    # Load the question, without the results tables, for processing.
    Q26 <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q26.rds'
      )
    )

    # Process the question and insert results tables into it.
    Q26 <- process_question_results(Q26, original_first_rows)

    # Load the previously computed results table.
    Q26_results_table <-  readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q26_results_table.rds'
      )
    )
    Q26_results_table <- as.data.frame(Q26_results_table)

    # Check that the previously computed results and new results match.
    expect_true(all(Q26[['Table']] == Q26_results_table) &&
                  all(names(Q26[['Table']]) == names(Q26_results_table)))
  }
)


# Long Exhaustive Sample Survey: Q22
test_that(
  "Test that mc_multiple_answer_results is correct for Q22 in the Long Exhaustive Sample Survey",
  {
    # Load the question, without the results tables, for processing.
    Q22 <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q22.rds'
      )
    )

    # Process the question and insert results tables into it.
    Q22 <- process_question_results(Q22, original_first_rows)

    # Load the previously computed results table.
    Q22_results_table <-  readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q22_results_table.rds'
      )
    )
    Q22_results_table <- as.data.frame(Q22_results_table)

    # Check that the previously computed results and new results match.
    expect_true(all(Q22[['Table']] == Q22_results_table) &&
                  all(names(Q22[['Table']]) == names(Q22_results_table)))
  }
)


# Long Exhaustive Sample Survey: Q20
test_that(
  "Test that mc_multiple_answer_results is correct for Q20 in the Long Exhaustive Sample Survey",
  {
    # Load the question, without the results tables, for processing.
    Q20 <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q20.rds'
      )
    )

    # Process the question and insert results tables into it.
    Q20 <- process_question_results(Q20, original_first_rows)

    # Load the previously computed results table.
    Q20_results_table <-  readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q20_results_table.rds'
      )
    )
    Q20_results_table <- as.data.frame(Q20_results_table)

    # Check that the previously computed results and new results match.
    expect_true(all(Q20[['Table']] == Q20_results_table) &&
                  all(names(Q20[['Table']]) == names(Q20_results_table)))
  }
)


