#' Convert HTML to a Docx File
#'
#' This function uses pandoc and tempfiles to create a docx version
#' of an HTML document. The HTML is prepended and appended with
#' <!doctype html><html><body> and </body></html>. Then it is saved
#' to a tempfile, and then pandoc converts it to a .docx file. Finally,
#' the path to the .docx file is returned.
#'
#' @param html an html body contents string. Do not include the <html><body> ... tags,
#' the html parameter will automatically have those kinds of tags added within this
#' function.
#' @param output_dir an optional parameter for specifying an output folder
#'
#' @return the path to the docx file created from converting the html.
html_2_pandoc <- function(html, file_name, format, output_dir) {
  # it is necessary that we save the file in UTF-8 format, otherwise Pandoc
  # will not be able to read it.
  options("encoding" = "UTF-8")

  # set default format to docx
  if (missing(format)) {
    if (missing(file_name)) {
      format <- "docx"
    } else {
      format <- tools::file_ext(file_name)
    }
  }

  # save the original working directory so we can return to it at the end.
  # move to the temporary files directory, use tempfile() to create
  # the temporary HTML document, and then the .docx file path which we will
  # use pandoc to generate.
  orig_directory <- getwd()

  temp_dir <- tempdir()
  setwd(temp_dir)
	html <- unlist(c("<!doctype html><html><body>", html, "</body></html>"))
	temp_html <- basename(tempfile(fileext = ".html"))
	temp_output_full <- tempfile(fileext = paste0(".", format))
	temp_output <- basename(temp_output_full)
	write(html, file = temp_html)

	# create the pandoc conversion command by replacing
	# the "temp_html" and "temp_docx" values with their
	# actual values (the file names).
	# run the pandoc conversion, then return to the original directory,
	# then return the full filepath of the converted docx file.
	pandoc_command <- "pandoc -s temp_html -o temp_output"
	pandoc_command <- gsub("temp_html", temp_html, pandoc_command)
	pandoc_command <- gsub("temp_output", temp_output, pandoc_command)
	system(pandoc_command)

	# rename the file if a name was given
	if (!missing(file_name)) {
	  file.rename(from=temp_output, to=file_name)
	  temp_output_full <- file.path(temp_dir, file_name)
	}

	if (!missing(output_dir)) {
	  file.rename(from=temp_output_full, to=file.path(output_dir, basename(temp_output_full)))
	  temp_output_full <- file.path(output_dir, basename(temp_output_full))
	}

	# make sure the path is safe for Windows
	temp_output_full <- gsub('\\\\', '/', temp_output_full)

	# set things back to normal
	setwd(orig_directory)
	options("encoding" = "native.enc")


	return(temp_output_full)
}

