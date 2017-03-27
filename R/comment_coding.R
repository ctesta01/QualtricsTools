#' Turn a Directory into a list of Coded Comment Data Frames (unprocessed)
directory_get_coded_comment_sheets <- function(directory) {

  # ask for directory if not provided
  if (missing(directory)) directory <- choose.dir()

  # we only want to look at Excel or CSV files in the given directory
  files_list <- list.files(path=directory, full.names=TRUE)
  files_list <- files_list[lapply(files_list, function(x) grepl("*.xlsx$|*.xls$|*.csv$", x)) == TRUE]
  files_list <- files_list[lapply(files_list, function(x) grepl("^~", basename(x))) == FALSE]


  # If there are warnings in reading in the Excel sheets,
  # Save them as we go. Additionally, save the corresponding sheet's filenames.
  # If everything goes smoothly, we will construct a list of sheets which
  # contain coded comments, called the coded_appendix_tables
  warnings_list <- list()
  warning_files_list <- list()
  coded_appendix_tables <- list()

  for (i in 1:length(files_list)) {

    # For each file in the files_list, try to get its coded comment sheet.
    # If it warns, save the error and filename to warnings_list and warning_files_list.
    tryCatch(coded_appendix_tables[[i]] <- get_coded_comment_sheet(files_list[[i]]),
             warning = function(w) {
               warning_files_list[[i]] <- files_list[[i]]
               warnings_list[[i]] <- w
             })

  }

  # Subset the warnings_list and warning_files_list's to only include non-empty entries
  warnings_list <- warnings_list[lapply(warnings_list, length) != 0]
  warning_files_list <- warning_files_list[lapply(warning_files_list, length) != 0]

  # Print the first warning for each sheet
  if (length(warning_files_list) > 0) {
    print("There were errors getting the coded comment sheets")
    print("Theses are the following files that had errors and the first error message for each.")

    for (i in 1:length(warning_files_list)) {
      # print the filename the warning is generated from
      print(warning_files_list[[i]])

      # If it was an 'expecting numeric' message, include a message explaining
      # that it's an issue with the typing of data in the columns in the Excel sheets
      numeric_message <- "expecting numeric: got"
      if (grepl(numeric_message, (warnings_list[[i]]))) {
        print("Some of the cells that should be stored as numeric are stored as strings")
        print("Please go back into the file and change them to numeric")
        print("Here is the first error message:")
      }

      # print the warning
      print(warnings_list[[i]])
    }

    # if warnings were returned, return NULL
    return(NULL)
  }

  return(coded_appendix_tables)
}

#' Turn a Single Coded File into a Data Frame
get_coded_comment_sheet <- function(codedfile){

  # Ask for the Coded File if there isn't one provided
  if (missing(codedfile)) codedfile <- file.choose()

  # Pick out the sheet called "Coded"
  # Error if there isn't one
  sheetindex <- which(tolower(readxl::excel_sheets(codedfile))=="coded")
  if (length(sheetindex) == 0) {
    cat(paste0(codedfile, " did not have a Coded tab\n"))
    return(NA)
  }

  # Load the Coded Comments as a Data Frame
  coded_orig <- readxl::read_excel(codedfile, sheet=sheetindex)

  # Strip out the Blank Rows
  blank_rows <- which(is.na(coded_orig[1]) | nchar(coded_orig[1])<5)
  if (length(blank_rows) == 0) {coded_use <- coded_orig}
  else if (length(blank_rows)>0) {coded_use <- coded_orig[-blank_rows,]}

  # Make sure the Coded Comments have a Varname column
  index_qname <- which(tolower(names(coded_use))=="varname")
  if (length(index_qname) == 0) {
    cat(paste0(codedfile, " did not have a varname column\n"))
    return(NA)
  }

  # Return the Coded Comments Data Frame (unprocessed)
  return(coded_use)
}

#' Turn the original coded comments sheet into a pair: (Question, Data Frame)
format_coded_comments <- function(coded_comment_sheet) {
  # determine which column to start with
  index_qname <- which(tolower(names(coded_comment_sheet))=="varname")

  # get the varname from the sheet
  varname = as.character(coded_comment_sheet[1,index_qname])

  # get coded comments, and the number of comments for each
  codeList <- names(coded_comment_sheet)[(index_qname+2):ncol(coded_comment_sheet)]
  numComments <- lapply (codeList, function(x) length(which(coded_comment_sheet[x]==1)))

  # construct the table
  coded_table <- as.data.frame(cbind(codeList,numComments,deparse.level=0))
  names(coded_table) <- c("Response", "N")

  # remove zeroes
  coded_table <- coded_table[coded_table['N'] != 0, ]

  # sort by reverse numerically twice
  # sorting the first time gives reverse numerically reverse alphabetically
  # sorting the second time reverses the reverse alphabetic to forward alphabetic,
  # while keeping the descending numerical sort
  coded_table <- coded_table[rev(order(unlist(coded_table[,'N']))),]
  coded_table <- coded_table[rev(order(unlist(coded_table[,'N']))),]


  # add "Total" and the total N to the list of coded comments and Ns
  n_comments <- length(unique(coded_comment_sheet[, 1]))
  coded_table <- rbind(coded_table,c("Total", n_comments))


  # we return a pair, the varname and the coded table.
  return(list(varname, coded_table))

}

#' Turn a List of Unprocessed Coded Comment Sheets into a List of Coded Comments Tables
format_coded_comment_sheets <- function(coded_comment_sheets) {
  coded_comments <- list()
  cc_length <- length(coded_comment_sheets)
  for (i in 1:cc_length) {
    coded_comments[[i]] <- format_coded_comments(coded_comment_sheets[[i]])
  }
  return(coded_comments)
}

#' Merge a Splitting Column into an Unprocessed Coded Comment Sheet
#'
#' Run create_merged_response_column() before this to create the splitting column in the responses.
#' Then pass the column name of the column created by create_merged_response_column() as split_column
#' @param split_column is the name of the column to merge in for splitting
merge_split_column_into_comment_sheet <- function(coded_comment_sheet, responses, split_column) {
  # Which column is the split_column
  split_index <- which(colnames(responses) == split_column)
  if (split_index==0) {
    # Error if the split_column isn't present
    stop("No column in responses with name ", split_column)
  }
  # Get the response IDs and the split_column into a 2-column data frame
  relevant_columns <- responses[, c(1, split_index)]
  colnames(relevant_columns)[[1]] <- colnames(responses)[[1]]

  coded_comment_sheet <- merge(x = coded_comment_sheet, y = relevant_columns, by = 1)
  split_index <- which(colnames(coded_comment_sheet) == split_column)
  re_ordering <- c(1, split_index, 2:(split_index-1))
  coded_comment_sheet <- coded_comment_sheet[,re_ordering]
  return(coded_comment_sheet)
}

# Format and Split a list of Unprocessed Coded Comment Sheets
format_and_split_comment_sheets <- function(coded_comment_sheets, responses, split_column) {

  # split_coded_comment_sheets will be a list of coded comment sheets for each respondent group
  levels <- levels(factor(responses[, split_column]))
  split_coded_comment_sheets <- sapply(levels, function(x) NULL)

  # merge split_column in and split each sheet
  for(i in 1:length(coded_comment_sheets)) {
    coded_comment_sheets[[i]] <- merge_split_column_into_comment_sheet(coded_comment_sheets[[i]], responses, split_column)
    coded_comment_sheets[[i]] <- split(coded_comment_sheets[[i]], coded_comment_sheets[[i]][, split_column], drop=TRUE)

    # sort each sheet into the appropriate level and insert into split_coded_comment_sheets
    for (j in 1:length(levels)) {
      sheet_contains_level <- sapply(coded_comment_sheets[[i]], function(x) isTRUE(levels[[j]] %in% x[, split_column]) )
      if (length(sheet_contains_level) != 0) {
        matching_split_sheet <- which(sheet_contains_level)
        if (length(matching_split_sheet) != 0) {
          split_coded_comment_sheets[[j]][[length(split_coded_comment_sheets[[j]]) + 1]] <- as.data.frame(coded_comment_sheets[[i]][[matching_split_sheet]])
        }
      }
    }
  }

  # Format each coded comment sheet
  for (i in 1:length(split_coded_comment_sheets)) {
    if (!is.null(split_coded_comment_sheets[[i]])) split_coded_comment_sheets[[i]] <- format_coded_comment_sheets(split_coded_comment_sheets[[i]])
  }

  return(split_coded_comment_sheets)
}

#' Insert Coded Comments into Blocks
insert_coded_comments <- function(blocks, original_first_rows, coded_comments) {

  r_col_dictionary <- create_response_column_dictionary(blocks, original_first_rows[1,])
  questions <- questions_from_blocks(blocks)

  for (i in 1:length(coded_comments)) {
    if (!is.null(coded_comments[[i]])) {
      varname <- coded_comments[[i]][[1]]
      matched_based_on_r_col <- which(r_col_dictionary[,2] == varname)
      if (length(matched_based_on_r_col) == 1) {
        varname <- r_col_dictionary[matched_based_on_r_col, 1]
      }
      question_index <- find_question_index(questions, varname)

      if (length(question_index) == 0) {
        cat(paste0("The appendices indicated for ", varname, " could not be matched to a question\n"))
        next;
      }
      cc_index <- length(questions[[question_index]][['CodedComments']]) + 1
      questions[[question_index]][['CodedComments']][[cc_index]] <- coded_comments[[i]]
    }
  }

  blocks <- questions_into_blocks(questions, blocks)
  return(blocks)
}

#' Split Survey and Insert Split Coded Comments
#'
#' The responses should already include the split column
insert_split_survey_comments <- function(split_blocks, split_coded_comment_sheets, split_column, original_first_rows) {
  # grab the original first rows if not included
  if (missing(original_first_rows)) original_first_rows <- get(x="original_first_rows", envir=globalenv())

  # match split blocks and split coded comments
  for (i in 1:length(split_coded_comment_sheets)) {
    if (!is.null(split_coded_comment_sheets[[i]])) {
      matching_block <- which(sapply(split_blocks, function(x) x[['split_group']] == names(split_coded_comment_sheets)[[i]]))
      split_blocks[[matching_block]] <- insert_coded_comments( split_blocks[[matching_block]], original_first_rows, split_coded_comment_sheets[[i]])
    }
  }
  return(split_blocks)
}


# Includes the comment coding pre-defined functions

generate_split_coded_comments <- function(qsf_file, csv_file, sheets_dir, output_dir, split_by) {

  # This turns the split_by list into a name for the column
  # which will contain the concatenation of the entries of responses
  # which are being split over. That is if split_by = c('column1', 'column2', 'column3'),
  # then this constructs split_string = 'column1-column2-column3'
  split_string <- toString(paste(split_by, "-"))
  split_string <- gsub(' ', '', split_string)
  split_string <- gsub(',', '', split_string)
  split_string <- substr(split_string, 1, nchar(split_string)-1)

  # Declares paths for the qsf and csv files
  get_setup(
    qsf_path = qsf_file,
    csv_path = csv_file,
    headerrows=3)

  # Merges the selected columns into one name
  # In this case School, DegType, and Porgram merged into school-degtype-program
  responses <- create_merged_response_column(split_by, split_string, blocks, responses)

  coded_sheets <- directory_get_coded_comment_sheets(sheets_dir)

  if (is.null(coded_sheets)) {
    stop("Please fix errors before attempting again")
  }

  split_comment_tables <- format_and_split_comment_sheets(coded_sheets, responses, split_string)
  split_blocks <- split_respondents(split_string, responses, survey, blocks, questions, headerrows=3)
  split_blocks <- insert_split_survey_comments(split_blocks, split_comment_tables, split_string, original_first_rows)
  output_dir <- out_dir

  #Used with html_2_pandoc below to keeps the flow of the survey consistent with the output
  flow = flow_from_survey(survey)

  #Appends .docx to the file names collected by splitting the data to output them as Word Documents
  filenames <- sapply(split_blocks, function(x) x$split_group)
  filenames <- sapply(filenames, function(x) paste0(x, '.docx'))

  #Outputs the data to word documents using html_2_pandoc
  for (i in 1:length(filenames)) {
    html_2_pandoc(
      html = c(blocks_header_to_html(split_blocks[[i]]),
               text_appendices_table(blocks = split_blocks[[i]],
                                     original_first_row = original_first_rows,
                                     flow = flow)),
      file_name = filenames[[i]],
      output_dir = output_dir
    )
  }

}
