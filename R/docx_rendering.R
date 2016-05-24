html_to_docx <- function(html) {
  options("encoding" = "UTF-8")
  orig_directory <- getwd()
  setwd(tempdir())
	html <- unlist(c("<!doctype html><html><body>", html, "</body></html>"))
	temp_html <- basename(tempfile(fileext = ".html"))
	temp_docx_full <- tempfile(fileext = ".docx")
	temp_docx <- basename(temp_docx_full)
	write(html, file = temp_html)
	pandoc_command <- "pandoc -s temp_html -o temp_docx"
	pandoc_command <- gsub("temp_html", temp_html, pandoc_command)
	pandoc_command <- gsub("temp_docx", temp_docx, pandoc_command)
	system(pandoc_command)
	setwd(orig_directory)
	return(temp_docx_full)
}

