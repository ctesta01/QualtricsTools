library(QualtricsTools)
library(XML)

context("Generating text appendices with inserted coded comments")

surveysdir = file.path(path.package('QualtricsTools'), 'data/Sample Surveys/')

test_that("Test that make_coded_comments includes the categories defined in the coded comments.", {

  requireNamespace("XML")

  # This test replicates the contents of the make_coded_comments function.
  # It tests several

  qsf_path = file.path(surveysdir, "/Better Sample Survey/Better_Sample_Survey.qsf")
  csv_path = file.path(surveysdir, "/Better Sample Survey/Better_Sample_Survey.csv")
  sheets_dir = file.path(surveysdir, "/Comment Coding/Coded Comments/")
  # output_file <- make_coded_comments(
  #   qsf_path = qsf,
  #   csv_path = csv,
  #   headerrows = 3,
  #   sheets_dir = sheets_dir,
  #   n_threshold = 0)

  headerrows <- 3
  get_setup_in_environment(
    qsf_path = qsf_path,
    csv_path = csv_path,
    headerrows = headerrows,
    environment = environment()
  )

  coded_sheets <- directory_get_coded_comment_sheets(sheets_dir)

  if (is.null(coded_sheets)) {
    stop("Please fix errors before attempting again")
  }

  comment_tables <-
    format_coded_comment_sheets(coded_comment_sheets = coded_sheets)
  blocks <-
    insert_coded_comments(
      blocks = blocks,
      original_first_rows = original_first_rows,
      coded_comments = comment_tables
    )

  # Used with html_2_pandoc below to keeps the flow of the survey consistent with the output
  flow = flow_from_survey(survey)

  output_html <- c(
    blocks_header_to_html(blocks),
    text_appendices_table(
      blocks = blocks,
      original_first_row = original_first_rows,
      flow = flow,
      n_threshold = 0
    )
  )

  output_file <- html_2_pandoc(
    html = output_html
  )

  expect_true(file.exists(output_file))
  html_tables <- XML::readHTMLTable(output_html)
  expect_true(length(html_tables) > 0)
  expect_true("Appendix Z" %in% unlist(html_tables))

})

