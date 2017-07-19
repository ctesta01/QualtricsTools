## FAQ
Q: My results tables look wrong. What could be causing it?

A: If you're generating them in the app, it could be that you've uploaded CSV data which does 
not match the formatting indicated by the "Insights?" checkbox. If you're working on the 
command line, I have made the mistake several times trying to generate reports without 
keeping my global scope tidy. I recommend clearing the environment variables before 
using QualtricsTools to generate a report.


Q: The results tables can't process my questions?

A: If your question is not one of the types currently supported by the application, it will 
not be processed and this will be noted in the output report. 
As of now, the QualtricsTools application only processes questions which are
multiple choice, likert matrix single or multiple answer, bipolar matrix, side-by-side, 
or text-entry questions. However, if it is one of these, there is probably a bug in the 
function corresponding to that question type in the `R/results_generation.R` file. 


Q: I can't export to Word or Excel, or it's taking too long. What should I do?

A: Sometimes exporting to Microsoft Office is difficult with large file sizes. My recommendation is to export your data to HTML and then "Open As" a Word or Excel document. Word can parse HTML, and from there you will just have to change the document layout from Web to Print if you'd like. 


Q: I need to clean my data, but I still want to be able to use QualtricsTools after cleaning it. What can I and can't I do?

A: Do not edit the column names of the responses CSV data. These are critical to how QualtricsTools pairs data from the QSF to the response data. However, you may 
remove complete rows and columns of CSV data without affecting QualtricsTools' ability to process your survey, and 
you may change responses as long as they are still valid responses to the provided question. Check out the [usage guide](https://github.com/ctesta01/QualtricsTools/wiki/Usage-Guidelines) to see more. 


Q: Qualtrics has many different ways to download data. Which format should I use if I want to use QualtricsTools on it?

A: Right now, you can download Qualtrics Surveys only as QSF files. However, the data can be downloaded in many different formats. QualtricsTools only supports CSV data (but it does support CSV data from both Legacy and Insights). Check out the appendix in this [usage guide](https://github.com/ctesta01/QualtricsTools/wiki/Usage-Guidelines) to see more. 


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


Q: The web browser says "File Not Found" or something similar when trying to download files from QualtricsTools. What's wrong? 

A: Likely either Pandoc or Rtools is not installed, or not in the system's `$PATH` variable. Check the StackOverflow post [Create zip file: error running command “ ” had status 127](https://stackoverflow.com/questions/29129681/create-zip-file-error-running-command-had-status-127/29480538#29480538) for instructions on setting up Rtools correctly. For installing Pandoc check out their [Installing Pandoc](http://pandoc.org/installing.html) page. Note that in order to render PDFs you need to additionally install pdflatex. 

Q: How do I get help on the functions in the QualtricsTools package? 

A: [The R-project's recommended ways to get help](https://www.r-project.org/help.html) are a good place to get started, but in addition to these I suggest using the RStudio's Help viewer. If you've opened the package as an RStudio project, then you can also use the file/function search in RStudio's toolbar.

![](http://i.imgur.com/9InH9Gd.png)

![](http://i.imgur.com/ZzvLEpc.png)

![](http://i.imgur.com/C2z13Eg.png)
