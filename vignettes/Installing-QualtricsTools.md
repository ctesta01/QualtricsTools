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
