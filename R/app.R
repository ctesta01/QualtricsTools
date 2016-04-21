#' A Shiny app to format Qualtrics survey data and generate reports
#'
#' This function launches the Shiny interface for the Qualtrics 
#' package from the files in the install or 'inst' directory. 
app = function() {
  shiny::runApp(system.file('shiny', package = 'qualtrics'))
}

