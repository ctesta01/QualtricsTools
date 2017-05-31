# QualtricsTools 

QualtricsTools is an R package that automatically processes Qualtrics survey data into 
reports showing the breakdowns of responses to each question. The package can create 
question response reports, appendices of free responses, question 
dictionaries, and even reports split into response based categories of respondents. 
This package uses the R web-application framework [Shiny](https://shiny.rstudio.com/), 
the universal document converter [Pandoc](http://pandoc.org/), 
[Roxygen2](https://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html) documentation, 
and much more. 

![A slideshow of the QualtricsTools web app](https://github.com/ctesta01/QualtricsTools/blob/master/pics/animation.gif?raw=true)

I have written this package for the Office of Institutional Research and Evaluation at Tufts University. However, 
anyone is welcome to use it.

## Installation
Before installing, you must install [R or Rstudio](https://www.rstudio.com/), 
[devtools](https://github.com/hadley/devtools), 
[Rtools](https://cran.r-project.org/bin/windows/Rtools/) (if you're on Windows), 
and [Pandoc](http://pandoc.org/). For Rtools with Windows, 
[please make sure Rtools is added to the `Path` environment variable](http://stackoverflow.com/a/29480538/3161979). 
After installing each of the prerequisites, to install QualtricsTools run the following in R:

    devtools::install_github("ctesta01/QualtricsTools")
    
To run the Shiny app, load the package and then call the app() function.

    library(QualtricsTools)
    QualtricsTools::app()

The QualtricsTools Shiny app should now be running! Enjoy. To update your version of QualtricsTools
to the most recent version, run `devtools::install_github("ctesta01/QualtricsTools")` again.

## Most Useful Functions

Here are some of the most high level functions in the application. Be sure to `library(QualtricsTools)` before 
trying to run any of these. Running these
commands without parameters like as follows results in interactive prompts for the survey data and other settings
in order to cut down on the need to repeatedly type or copy long file paths. 
Check out the documentation for how to use each of these explicitly. 

    # Load and Process Survey Data into R
    get_setup()
    
    # Start and run the Shiny app
    app()
    
    # Create a Report of Question Results Tables
    make_results_tables()
    
    # Create a Report of Text Appendices, for each free response part of the survey
    make_text_appendices()
    

## Usage and Reference Guides

The functionality of the web application and R package are documented in the following guides. Beyond this,
almost all functions have Roxygen generated documentation which means that after running `library(QualtricsTools)`
you can run `help(function)` or `?function` on any function in QualtricsTools to check out the Roxygen2 generated 
documentation. 

- How do I use the app?
  - [The Shiny Web App](https://github.com/ctesta01/QualtricsTools/wiki/The-Shiny-Web-Application#explaining-the-shiny-app-components)
  - [Running the App](https://github.com/ctesta01/QualtricsTools/wiki/Installing-and-Running-the-Shiny-App)
  - [Usage Guidelines](https://github.com/ctesta01/QualtricsTools/wiki/Usage-Guidelines)
- How do I use the R package?
  - [Generating Results Tables](https://github.com/ctesta01/QualtricsTools/wiki/Generating-Results-Tables)
  - [Generating Question Dictionaries](https://github.com/ctesta01/QualtricsTools/wiki/Generating-Question-Dictionaries)
  - [Generating the Unprocessed Questions Dictionary](https://github.com/ctesta01/QualtricsTools/wiki/Uncodeable-Questions)
  - [Generating Text Appendices](https://github.com/ctesta01/QualtricsTools/wiki/Generating-Text-Appendices)
  - [Generating Survey Logic](https://github.com/ctesta01/QualtricsTools/wiki/Generating-Display-Logic)
  - [Generating Split Reports by Responses](https://github.com/ctesta01/QualtricsTools/wiki/Split-Reports)
  - [Reshaping for Tableau](https://github.com/ctesta01/QualtricsTools/wiki/Reshaping-Responses-for-Tableau)
  - [Inserting Coded Comments into Text Appendices](https://github.com/ctesta01/QualtricsTools/wiki/Comment-Coding)
  - [Including/Excluding  Questions from Reports](https://github.com/ctesta01/QualtricsTools/wiki/Including-Excluding-a-Specific-Question)
  - [Processing a Specific Question](https://github.com/ctesta01/QualtricsTools/wiki/Processing-a-Specific-Question)
  - [Using Pandoc to Export to Different Filetypes](https://github.com/ctesta01/QualtricsTools/wiki/Exporting-to-Different-Filetypes)
- How does it work? (Reference)
  - [Understanding the Qualtrics Survey File (QSF)](https://gist.github.com/ctesta01/d4255959dace01431fb90618d1e8c241)
  - [The Package Source Code Layout](https://github.com/ctesta01/QualtricsTools/wiki/Source-Code-Layout)
  - [The Shiny App's Code](https://github.com/ctesta01/QualtricsTools/wiki/The-Shiny-Web-Application#understanding-the-code)

## Frequently Asked Questions 
Check out our [FAQ](https://github.com/ctesta01/QualtricsTools/wiki/Frequently-Asked-Questions) for more help.


