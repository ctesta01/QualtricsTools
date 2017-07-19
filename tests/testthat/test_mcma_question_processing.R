# Test mc_single_answer_results
context("Testing mc_multiple_answer_results")

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

# Long Exhaustive Sample Survey: Q9
test_that(
  "Test that mc_multiple_answer_results is correct for Q9 in the Long Exhaustive Sample Survey",
  {
    # Load the question, without the results tables, for processing.
    Q9 <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q9.rds'
      )
    )

    # Process the question and insert results tables into it.
    Q9 <- process_question_results(Q9, original_first_rows)

    # Load the previously computed results table.
    Q9_results_table <-  readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q9_results_table.rds'
      )
    )
    Q9_results_table <- as.data.frame(Q9_results_table)

    # Check that the previously computed results and new results match.
    expect_true(all(Q9[['Table']][["N"]] == Q9_results_table[["N"]]) &&
                  all(Q9[['Table']][["Percent"]] == Q9_results_table[["Percent"]]) &&
    all(names(Q9[['Table']]) == names(Q9_results_table)))
  }
)

# Long Exhaustive Sample Survey: Q21
test_that(
  "Test that mc_multiple_answer_results is correct for Q21 in the Long Exhaustive Sample Survey",
  {
    # Load the question, without the results tables, for processing.
    Q21 <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q21.rds'
      )
    )

    # Process the question and insert results tables into it.
    Q21 <- process_question_results(Q21, original_first_rows)

    # Load the previously computed results table.
    Q21_results_table <-  readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q21_results_table.rds'
      )
    )
    Q21_results_table <- as.data.frame(Q21_results_table)

    # Check that the previously computed results and new results match.
    expect_true(all(Q21[['Table']][["N"]] == Q21_results_table[["N"]]) &&
                  all(Q21[['Table']][["Percent"]] == Q21_results_table[["Percent"]]) &&
                  all(names(Q21[['Table']]) == names(Q21_results_table)))
  }
)

# Long Exhaustive Sample Survey: q13_family_food
test_that(
  "Test that mc_multiple_answer_results is correct for q13_family_food in the Long Exhaustive Sample Survey",
  {
    # Load the question, without the results tables, for processing.
    q13_family_food <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'q13_family_food.rds'
      )
    )

    # Process the question and insert results tables into it.
    q13_family_food <- process_question_results(q13_family_food, original_first_rows)

    # Load the previously computed results table.
    q13_family_food_results_table <-  readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'q13_family_food_results_table.rds'
      )
    )
    q13_family_food_results_table <- as.data.frame(q13_family_food_results_table)

    # Check that the previously computed results and new results match.
    expect_true(all(q13_family_food[['Table']][["N"]] == q13_family_food_results_table[["N"]]) &&
                  all(q13_family_food[['Table']][["Percent"]] == q13_family_food_results_table[["Percent"]]) &&
                  all(names(q13_family_food[['Table']]) == names(q13_family_food_results_table)))
  }
)

# Long Exhaustive Sample Survey: Q14
test_that(
  "Test that mc_multiple_answer_results is correct for Q14 in the Long Exhaustive Sample Survey",
  {
    # Load the question, without the results tables, for processing.
    Q14 <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q14.rds'
      )
    )

    # Process the question and insert results tables into it.
    Q14 <- process_question_results(Q14, original_first_rows)

    # Load the previously computed results table.
    Q14_results_table <-  readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q14_results_table.rds'
      )
    )
    Q14_results_table <- as.data.frame(Q14_results_table)

    # Check that the previously computed results and new results match.
    expect_true(all(Q14[['Table']][['N']] == Q14_results_table[["N"]]) &&
                  all(Q14[['Table']][['Percent']] == Q14_results_table[["Percent"]]) &&
                  all(names(Q14[['Table']]) == names(Q14_results_table)))
  }
)

# Long Exhaustive Sample Survey: Q15
test_that(
  "Test that mc_multiple_answer_results is correct for Q15 in the Long Exhaustive Sample Survey",
  {
    # Load the question, without the results tables, for processing.
    Q15 <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q15.rds'
      )
    )

    # Process the question and insert results tables into it.
    Q15 <- process_question_results(Q15, original_first_rows)

    # Load the previously computed results table.
    Q15_results_table <-  readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q15_results_table.rds'
      )
    )
    Q15_results_table <- as.data.frame(Q15_results_table)

    # Check that the previously computed results and new results match.
    expect_true(all(Q15[['Table']][["N"]] == Q15_results_table[["N"]]) &&
                  all(Q15[['Table']][["Percent"]] == Q15_results_table[["Percent"]]) &&
                  all(names(Q15[['Table']]) == names(Q15_results_table)))
  }
)


# Long Exhaustive Sample Survey: Q16
test_that(
  "Test that mc_multiple_answer_results is correct for Q16 in the Long Exhaustive Sample Survey",
  {
    # Load the question, without the results tables, for processing.
    Q16 <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q16.rds'
      )
    )

    # Process the question and insert results tables into it.
    Q16 <- process_question_results(Q16, original_first_rows)

    # Load the previously computed results table.
    Q16_results_table <-  readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q16_results_table.rds'
      )
    )
    Q16_results_table <- as.data.frame(Q16_results_table)

    # Check that the previously computed results and new results match.
    expect_true(all(Q16[['Table']][['N']] == Q16_results_table[['N']]) &&
                  all(Q16[['Table']][['Percent']] == Q16_results_table[['Percent']]) &&
                  all(names(Q16[['Table']]) == names(Q16_results_table)))
  }
)


# Long Exhaustive Sample Survey: Q12
test_that(
  "Test that mc_multiple_answer_results is correct for Q12 in the Long Exhaustive Sample Survey",
  {
    # Load the question, without the results tables, for processing.
    Q12 <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q12.rds'
      )
    )

    # Process the question and insert results tables into it.
    Q12 <- process_question_results(Q12, original_first_rows)

    # Load the previously computed results table.
    Q12_results_table <-  readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q12_results_table.rds'
      )
    )
    Q12_results_table <- as.data.frame(Q12_results_table)

    # Check that the previously computed results and new results match.
    expect_true(all(Q12[['Table']][['N']] == Q12_results_table[['N']]) &&
                  all(Q12[['Table']][['Percent']] == Q12_results_table[['Percent']]) &&
                  all(names(Q12[['Table']]) == names(Q12_results_table)))
  }
)
