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
html_to_docx <- function(html, file_name) {
  # it is necessary that we save the file in UTF-8 format, otherwise Pandoc
  # will not be able to read it.
  options("encoding" = "UTF-8")

  # save the original working directory so we can return to it at the end.
  # move to the temporary files directory, use tempfile() to create
  # the temporary HTML document, and then the .docx file path which we will
  # use pandoc to generate.
  orig_directory <- getwd()
  setwd(tempdir())
	html <- unlist(c("<!doctype html><html><body>", html, "</body></html>"))
	temp_html <- basename(tempfile(fileext = ".html"))
	temp_docx_full <- tempfile(fileext = ".docx")
	temp_docx <- basename(temp_docx_full)
	write(html, file = temp_html)

	# create the pandoc conversion command by replacing
	# the "temp_html" and "temp_docx" values with their
	# actual values (the file names).
	# run the pandoc conversion, then return to the original directory,
	# then return the full filepath of the converted docx file.
	pandoc_command <- "pandoc -s temp_html -o temp_docx"
	pandoc_command <- gsub("temp_html", temp_html, pandoc_command)
	pandoc_command <- gsub("temp_docx", temp_docx, pandoc_command)
	system(pandoc_command)

	# rename the file if a name was given
	if (!missing(file_name)) {
	  file.rename(from=temp_docx, to=file_name)
	  temp_docx_full <- file.path(tempdir(), file_name)
	}

	# make sure the path is safe for Windows
	temp_docx_full <- gsub('\\\\', '/', temp_docx_full)

	# set things back to normal
	setwd(orig_directory)
	options("encoding" = "native.enc")


	return(temp_docx_full)
}

