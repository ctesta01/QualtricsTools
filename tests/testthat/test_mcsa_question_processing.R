
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
    expect_true(all(q2_favorite[['Table']] == q2_favorite_results_table))

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
    expect_true(all(Q2[['Table']] == Q2_results_table))
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
    expect_true(all(Q10[['Table']] == Q10_results_table))

  }
)


# Long Exhaustive Sample Survey: Q11
test_that(
  "Test that mc_single_answer_results is correct for Q11 in the Long Exhaustive Sample Survey",
  {
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
    expect_true(all(Q11[['Table']] == Q11_results_table))
  }
)


# Long Exhaustive Sample Survey: Q3
test_that("Test that mc_single_answer_results is correct for Q3 in the Long Exhaustive Sample Survey", {
  # Load the question, without the results tables, for processing.
  Q3 <- readRDS(
    file.path(
      find.package('QualtricsTools'),
      'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
      'Q3.rds'
    )
  )

  # Process the question and insert results tables into it.
  Q3 <- process_question_results(Q3, original_first_rows)

  # Load the previously computed results table.
  Q3_results_table <-
    readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q3_results_table.rds'
      )
    )
  Q3_results_table <- as.data.frame(Q3_results_table)

  # Check that the previously computed results and new results match.
  expect_true(all(Q3[['Table']] == Q3_results_table))
})


# Long Exhaustive Sample Survey: q4_colored_fruit
test_that("Test that mc_single_answer_results is correct for q4_colored_fruit in the Long Exhaustive Sample Survey", {
  # Load the question, without the results tables, for processing.
  q4_colored_fruit <- readRDS(
    file.path(
      find.package('QualtricsTools'),
      'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
      'q4_colored_fruit.rds'
    )
  )

  # Process the question and insert results tables into it.
  q4_colored_fruit <- process_question_results(q4_colored_fruit, original_first_rows)

  # Load the previously computed results table.
  q4_colored_fruit_results_table <-
    readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'q4_colored_fruit_results_table.rds'
      )
    )
  q4_colored_fruit_results_table <- as.data.frame(q4_colored_fruit_results_table)

  # print.data.frame(q4_colored_fruit_results_table)
  # print.data.frame(q4_colored_fruit[['Table']])
  # print(str(q4_colored_fruit_results_table))
  # print(str(as.data.frame(q4_colored_fruit[['Table']], stringsAsFactors=FALSE)))
  # print(options()[['stringsAsFactors']])

  # Check that the previously computed results and new results match.
  expect_true(all(q4_colored_fruit[['Table']] == q4_colored_fruit_results_table) &&
                all(
                  names(q4_colored_fruit[['Table']]) == names(q4_colored_fruit_results_table)
                ))
})


# Long Exhaustive Sample Survey: Q5
test_that("Test that mc_single_answer_results is correct for Q5 in the Long Exhaustive Sample Survey", {
  # Load the question, without the results tables, for processing.
  Q5 <- readRDS(
    file.path(
      find.package('QualtricsTools'),
      'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
      'Q5.rds'
    )
  )

  # Process the question and insert results tables into it.
  Q5 <- process_question_results(Q5, original_first_rows)

  # Load the previously computed results table.
  Q5_results_table <-
    readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q5_results_table.rds'
      )
    )
  Q5_results_table <- as.data.frame(Q5_results_table)

  # print.data.frame(Q5_results_table)
  # print.data.frame(Q5[['Table']])
  # print(str(Q5_results_table))
  # print(str(as.data.frame(Q5[['Table']], stringsAsFactors=FALSE)))
  # print(options()[['stringsAsFactors']])

  # Check that the previously computed results and new results match.
  expect_true(all(Q5[['Table']] == Q5_results_table) &&
                all(names(Q5[['Table']]) == names(Q5_results_table)))
})


# Long Exhaustive Sample Survey: Q6
test_that("Test that mc_single_answer_results is correct for Q6 in the Long Exhaustive Sample Survey", {
  # Load the question, without the results tables, for processing.
  Q6 <- readRDS(
    file.path(
      find.package('QualtricsTools'),
      'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
      'Q6.rds'
    )
  )

  # Process the question and insert results tables into it.
  Q6 <- process_question_results(Q6, original_first_rows)

  # Load the previously computed results table.
  Q6_results_table <-
    readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q6_results_table.rds'
      )
    )
  Q6_results_table <- as.data.frame(Q6_results_table)

  # print.data.frame(Q6_results_table)
  # print.data.frame(Q6[['Table']])
  # print(str(Q6_results_table))
  # print(str(as.data.frame(Q6[['Table']], stringsAsFactors=FALSE)))
  # print(options()[['stringsAsFactors']])

  # Check that the previously computed results and new results match.
  expect_true(all(Q6[['Table']] == Q6_results_table) &&
                all(names(Q6[['Table']]) == names(Q6_results_table)))
})


# Long Exhaustive Sample Survey: Q7
test_that("Test that mc_single_answer_results is correct for Q7 in the Long Exhaustive Sample Survey", {
  # Load the question, without the results tables, for processing.
  Q7 <- readRDS(
    file.path(
      find.package('QualtricsTools'),
      'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
      'Q7.rds'
    )
  )

  # Process the question and insert results tables into it.
  Q7 <- process_question_results(Q7, original_first_rows)

  # Load the previously computed results table.
  Q7_results_table <-
    readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q7_results_table.rds'
      )
    )
  Q7_results_table <- as.data.frame(Q7_results_table)

  # print.data.frame(Q7_results_table)
  # print.data.frame(Q7[['Table']])
  # print(str(Q7_results_table))
  # print(str(as.data.frame(Q7[['Table']], stringsAsFactors=FALSE)))
  # print(options()[['stringsAsFactors']])

  # Check that the previously computed results and new results match.
  expect_true(all(Q7[['Table']] == Q7_results_table) &&
                all(names(Q7[['Table']]) == names(Q7_results_table)))
})


