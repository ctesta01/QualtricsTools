
# Test mc_single_answer_results
context("Testing mc_single_answer_results")


# Better Sample Survey: q2_favorite
test_that(
  "Test that mc_single_answer_results is correct for q2_favorite in the Better Sample Survey",
  {
    # Load the original first rows from the Better Sample Survey
    original_first_rows <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Better Sample Survey/RDS',
        'original_first_rows.rds'
      )
    )

    # Load the question, without the results tables, for processing.
    q2_favorite <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Better Sample Survey/RDS',
        'q2_favorite.rds'
      )
    )

    # Process the question and insert results tables into it.
    q2_favorite <-
      process_question_results(q2_favorite, original_first_rows)

    # Load the previously computed results table.
    q2_favorite_results_table <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Better Sample Survey/RDS',
        'q2_favorite_results_table.rds'
      )
    )
    q2_favorite_results_table <- as.data.frame(q2_favorite_results_table)

    # Check that the previously computed results and new results match.
    expect_identical(q2_favorite[['Table']], q2_favorite_results_table)
  }
)

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

# Long Exhaustive Sample Survey: Q2
test_that(
  "Test that mc_single_answer_results is correct for Q2 in the Long Exhaustive Sample Survey",
  {
    # Load the question, without the results tables, for processing.
    Q2 <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q2.rds'
      )
    )

    # Process the question and insert results tables into it.
    Q2 <- process_question_results(Q2, original_first_rows)

    # Load the previously computed results table.
    Q2_results_table <-  readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q2_results_table.rds'
      )
    )
    Q2_results_table <- as.data.frame(Q2_results_table)

    # Check that the previously computed results and new results match.
    expect_identical(Q2[['Table']], Q2_results_table)
  }
)


# Long Exhaustive Sample Survey: Q10
test_that(
  "Test that mc_single_answer_results is correct for Q10 in the Long Exhaustive Sample Survey",
  {
    # Load the question, without the results tables, for processing.
    Q10 <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q10.rds'
      )
    )

    # Process the question and insert results tables into it.
    Q10 <- process_question_results(Q10, original_first_rows)

    # Load the previously computed results table.
    Q10_results_table <-
      readRDS(
        file.path(
          find.package('QualtricsTools'),
          'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
          'Q10_results_table.rds'
        )
      )
    Q10_results_table <- as.data.frame(Q10_results_table)

    # Check that the previously computed results and new results match.
    expect_identical(Q10[['Table']], Q10_results_table)
  }
)


# Long Exhaustive Sample Survey: Q11
test_that(
  "Test that mc_single_answer_results is correct for Q11 in the Long Exhaustive Sample Survey",
  {
    original_first_rows <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'original_first_rows.rds'
      )
    )

    # Load the question, without the results tables, for processing.
    Q11 <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q11.rds'
      )
    )

    # Process the question and insert results tables into it.
    Q11 <- process_question_results(Q11, original_first_rows)

    # Load the previously computed results table.
    Q11_results_table <-
      readRDS(
        file.path(
          find.package('QualtricsTools'),
          'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
          'Q11_results_table.rds'
        )
      )

    # Check that the previously computed results and new results match.
    # identical(Q11[['Table']], Q11_results_table)
    # expect_true(all.equal(Q11[['Table']], Q11_results_table))
    # expect_identical(Q11[['Table']], Q11_results_table)
    # expect_equivalent(Q11[['Table']], Q11_results_table)
    expect_true(all(Q11[['Table']] == Q11_results_table))
  }
)

