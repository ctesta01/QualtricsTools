# Test mc_single_answer_results

context("Testing mc_single_answer_results")
rm(list = ls()) # clear the environment
# surveysdir is the sample surveys filepath
surveysdir = file.path(path.package('QualtricsTools'), 'data/Sample Surveys/')

# Better Sample Survey: q2_favorite
test_that(
  "Test that mc_single_answer_results is correct for q2_favorite in the Better Sample Survey",
  {
    original_first_rows <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Better Sample Survey/RDS',
        'original_first_rows.rds'
      )
    )
    q2_favorite <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Better Sample Survey/RDS',
        'q2_favorite.rds'
      )
    )
    q2_favorite <-
      process_question_results(q2_favorite, original_first_rows)
    q2_favorite_results_table <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Better Sample Survey/RDS',
        'q2_favorite_results_table.rds'
      )
    )
    expect_identical(q2_favorite[['Table']], q2_favorite_results_table)
  }
)

# Long Exhaustive Sample Survey: Q2
test_that(
  "Test that mc_single_answer_results is correct for Q2 in the Long Exhaustive Sample Survey",
  {
    original_first_rows <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'original_first_rows.rds'
      )
    )
    Q2 <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q2.rds'
      )
    )
    Q2 <- process_question_results(Q2, original_first_rows)
    Q2_results_table <-  readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q2_results_table.rds'
      )
    )
    expect_identical(Q2[['Table']], Q2_results_table)
  }
)

test_that(
  "Test that mc_single_answer_results is correct for Q10 in the Long Exhaustive Sample Survey",
  {
    survey_dir = file.path(surveysdir, "Long Exhaustive Sample Survey")
    qsf = file.path(survey_dir, "Long_Exhaustive_Sample_Survey.qsf")
    csv = file.path(survey_dir, "Long_Exhaustive_Sample_Survey.csv")
    # Use capture.output to suppress printed messages from get_setup in testing.
    # Syntax from https://stackoverflow.com/questions/2723034/suppress-one-commands-output-in-r
    capture.output(get_setup(
      headerrows = 3,
      qsf_path = qsf,
      csv_path = csv
    ), file = 'NUL')

    index = find_question_index(questions, 'Q10')
    df = questions[[index]][['Table']]
    comparison_df <-
      readRDS(
        file.path(
          find.package('QualtricsTools'),
          'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
          'Q10_results_table.rds'
        )
      )
    expect_identical(df, comparison_df)
  }
)
