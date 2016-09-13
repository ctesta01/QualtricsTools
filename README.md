### the QualtricsTools project

This is a project I'm working on to automate creating reports from 
Qualtrics survey data and responses. This is an R package that 
comes with a Shiny application that can be used for automatic report generation. 

[![screenshot of application](https://i.imgur.com/A4zQizk.png)](https://www.youtube.com/watch?v=rmUC5EVGJiU)

Checkout my [YouTube video](https://www.youtube.com/watch?v=rmUC5EVGJiU) for an informal overview of the application. 

#### installing

To get setup, you'll need to have R or [Rstudio](https://www.rstudio.com/), [devtools](https://github.com/hadley/devtools), [Rtools](https://cran.r-project.org/bin/windows/Rtools/) if you're on Windows, and [Pandoc](http://pandoc.org/installing.html).

    devtools::install_github("ctesta01/QualtricsTools")
    library(QualtricsTools)
    QualtricsTools::app()

The QualtricsTools Shiny app should now be running! Enjoy!

#### more to come soon!

If you have questions, please feel free to email me at christian.testa@tufts.edu. Otherwise, the [wiki](https://github.com/ctesta01/QualtricsTools/wiki/) has some neat stuff. 

