# QualtricsTools

QualtricsTools is a package and web application that allows users to process
Qualtrics Survey Files (QSF) and survey response data into results tables, 
question dictionaries, text appendices and more. The application and package
were built using R, Shiny, and Pandoc.

![A slideshow of the QualtricsTools web app](https://github.com/ctesta01/QualtricsTools/blob/master/pics/animation.gif?raw=true)

## What does QualtricsTools do? (Features)
The QualtricsTools app can be run as a local web-application which acts as
a graphical interface to the tools built into the package. After uploading
a QSF and CSV, it calculates frequencies and percentage responses to the 
questions in a survey, tables relevant information about the questions, 
includes a description of the survey logic and flow, and lists all
free response answers to questions in the text appendices. All of this data
can be readily exported into a number of different formats (xlsx, csv, docx,
html, md, pdf, and more) facilitated by document conversion through pandoc. 

The R package that the app is built on top of provides a richer set 
of functionalities including automatically reshaping the response data
from long to lean format, stitching categorical comment coding data into
a survey's text appendices, and more. The package is intended to provide
an interface in R through which one can begin to work with Qualtrics survey
and response data.

## Installation
Before installing, you must install [R or Rstudio](https://www.rstudio.com/), 
[devtools](https://github.com/hadley/devtools), 
[Rtools](https://cran.r-project.org/bin/windows/Rtools/) (if you're on Windows), 
and [Pandoc](http://pandoc.org/). Follow the previous links, and there 
are directions on each page on how to install them. For Rtools with Windows, 
[please make sure Rtools is added to the `Path` environment variable](http://stackoverflow.com/a/29480538/3161979). 
After installing each of these, run the following commands in R:

    devtools::install_github("ctesta01/QualtricsTools")
    library(QualtricsTools)
    QualtricsTools::app()

The QualtricsTools Shiny app should now be running! Enjoy!

To update your version of QualtricsTools, just run `devtools::install_github("ctesta01/QualtricsTools")` again.

## Usage and Reference Guides

The functionality of the web application and R package are documented in the following guides. Beyond this,
almost all functions have Roxygen generated documentation which means that after running `library(QualtricsTools)`
you can run `help(function)` on any function in QualtricsTools, like `help(get_setup)`, to check out the
documentation pages.

- How do I use the app?
  - [Running the App](https://github.com/ctesta01/QualtricsTools/wiki/Installing-and-Running-the-Shiny-App)
  - [Parts of the App](https://github.com/ctesta01/QualtricsTools/wiki/Parts-of-the-Shiny-App)
  - [Usage Guidelines](https://github.com/ctesta01/QualtricsTools/wiki/Usage-Guidelines)
- How do I use the R package?
  - [Generating Results Tables](https://github.com/ctesta01/QualtricsTools/wiki/Generating-Results-Tables)
  - [Generating Question Dictionaries](https://github.com/ctesta01/QualtricsTools/wiki/Generating-Question-Dictionaries)
  - [Generating Text Appendices](https://github.com/ctesta01/QualtricsTools/wiki/Generating-Text-Appendices)
  - [Generating Survey Logic](https://github.com/ctesta01/QualtricsTools/wiki/Generating-Display-Logic)
  - [Split Reports by Responses](https://github.com/ctesta01/QualtricsTools/wiki/Split-Reports)
  - [Tableau Reshaping](https://github.com/ctesta01/QualtricsTools/wiki/Reshaping-Responses-for-Tableau)
  - [Question Dictionary](https://github.com/ctesta01/QualtricsTools/wiki/Generating-Question-Dictionaries)
  - [Uncodeable Questions Dictionary](https://github.com/ctesta01/QualtricsTools/wiki/Uncodeable-Questions)
  - [Comment Coding](https://github.com/ctesta01/QualtricsTools/wiki/Comment-Coding)
  - [Including/Excluding  Questions](https://github.com/ctesta01/QualtricsTools/wiki/Including-Excluding-a-Specific-Question)
  - [Processing a Specific Question](https://github.com/ctesta01/QualtricsTools/wiki/Processing-a-Specific-Question)
  - [Exporting to Different Filetypes](https://github.com/ctesta01/QualtricsTools/wiki/Exporting-to-Different-Filetypes)
- How does it work? (Reference)
  - [Understanding the QSF](https://gist.github.com/ctesta01/d4255959dace01431fb90618d1e8c241)
  - [Understanding the source code](https://github.com/ctesta01/QualtricsTools/wiki/Source-Code-Layout)

## FAQ
Q: My results tables look wrong. What could be causing it?

A: If you're generating them in the app, it could be that you've uploaded CSV data which does 
not match the formatting indicated by the "Insights?" checkbox. If you're working on the 
command line, I have made the mistake several times trying to generate reports without 
keeping my global scope tidy. I recommend clearing the environment variables before 
using QualtricsTools to generate a report.


Q: The results tables can't process my questions?

A: As of now, the QualtricsTools application only processes questions which are
multiple choice, likert matrix single or multiple answer, bipolar matrix, side-by-side, 
or text-entry questions. 


Q: I can't export to Word or Excel, or it's taking too long. What should I do?

A: Sometimes I think Word has trouble with large file sizes. My recommendation is to export your data to HTML and then "Open As" a Word document. Word can parse HTML, and from there you will just have to change the document layout to Print from Web. The same trick works for Excel, but one could also export to a CSV sheet rather than an Excel document in most cases.


Q: I need to clean my data, but I still want to be able to use QualtricsTools after cleaning it. What can and can't I do?

A: I would advise against editing the QSF and the column names of the CSV dataset. These are crucial structural components to the survey, and are integral in how QualtricsTools processes the survey data. However, you may 
remove complete rows and columns of respondent data without affecting QualtricsTools' ability to process your survey, and 
you may change responses as long as they are still valid responses to the provided question. Check out the [usage guide](https://github.com/ctesta01/QualtricsTools/wiki/Usage-Guidelines) to see more. 


Q: Qualtrics has many different ways to download data. Which format should I use if I want to use QualtricsTools on it?

A: Right now, you can download Qualtrics Surveys only as QSF files. However, the data can be downloaded in many different formats. QualtricsTools only supports CSV data, but it supports data from both Legacy and Insights. Check out the appendix in this [usage guide](https://github.com/ctesta01/QualtricsTools/wiki/Usage-Guidelines) to see more. 


Q: When trying to export to xlsx or reshaping my data to lean data, R is unable to find 
Perl even though I installed it? 

A: Perl is necessary for the readxl, gdata, WriteXLS, and other package to work correctly. Further, 
R can only find Perl if it's in an operating system environment variable called the `PATH`.
The process for adding the path to Perl on Windows is similar to [the process 
for adding Rtools to the `PATH`](http://stackoverflow.com/a/29480538/3161979), but using 
something more like "C:/Perl64/bin/perl" or a similar string. Analogous steps for ensuring
Rtools and Perl are in the `PATH` need to be taken on any operating system. 


Q: My survey's ordering in the results tables or text appendices seems wrong or is missing parts. 
Why?

A: Does your survey contain particularly complex flow or logic? If so, and your survey contains
things like randomized logic or branching, then your best bet is to process the survey without
using the survey flow data. QualtricsTools only very naively can use the survey flow to 
order the survey data, and it doesn't know what to do with more complex examples, so the ordering
will have to be left to after outputting the data. 


Q: How do I get help on the functions in the QualtricsTools package? 

A: [The R-project's recommended ways to get help](https://www.r-project.org/help.html) are recommended, but in addition to these I suggest using the RStudio's Help viewer. If you've opened the package as an RStudio project, then you can use the file/function search in RStudio (this can be opened with Ctrl+. or found in RStudio's toolbar).

![](http://i.imgur.com/9InH9Gd.png)

![](http://i.imgur.com/ZzvLEpc.png)

![](http://i.imgur.com/C2z13Eg.png)
