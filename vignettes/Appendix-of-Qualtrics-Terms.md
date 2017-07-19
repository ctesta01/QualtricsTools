# Appendix
### Legacy and Insights Data

Below, I am showing the process for exporting data in the Insights platform for use with the QualtricsTools package. In Insights, you should export a CSV complete data set with the "use legacy View Results format" box unchecked to get Insights-formatted data. If the box is checked, or you are using the legacy platform, you will get legacy formatted data. 

[[http://i.imgur.com/Csydpki.png | width=200px]]

[[http://i.imgur.com/DLSC2wc.png | width=400px]]

[[http://i.imgur.com/aMus7uE.png | width=600px]]

---
### Data Export Tags
Data Export Tags are the user defined names for each question. You can edit them in Qualtrics.

![editing a data export tag](http://i.imgur.com/3X9SJu6.png)

---
### Recode Values
Recode Values are the naming that a user can assign to part of a question. In most cases, Recode Values are numerical, and are used both as a question part's representative in response column headers and in the response data entries. If you want to see more specific examples of how recode values can be used, please feel free to check out [my gist of recode values examples by question type](https://gist.github.com/ctesta01/9763dc35eee5301feb30c4fbfce6d2a0)

[[http://i.imgur.com/80iKEqS.png | width=400px]]

[[http://i.imgur.com/2uczrrW.png | width=400px]]

---
### Question Export Tags
Question Export Tags are a specific kind of Recode Value, available only on some question types through the Recode Values menu option. Question Export Tags can be set as strings, and most of the time correspond to the horizontal question components in more complex question types, like Likert Matrix questions and Side-by-Side questions. 

[[http://i.imgur.com/XxYDWFn.png | width=400px]]

---
### Response Set Header Rows
The header rows are very necessary for many different parts of the program. Generally, they must be present and accurate for the program to work correctly. "Accurate" meaning that they accurately reflect the part of the Qualtrics survey they correspond to, and that they contain the correct information in the question preview row (2nd row) and the import IDs (3rd row, if present). 

[[http://i.imgur.com/EyquDP3.png | width=600px]]

---
### Question IDs and Import IDs
The Question IDs and Import IDs are Qualtrics' internal naming for questions and question parts. The Import IDs are visible in the third row of in the header rows (see above), and optionally you can set the Data Export Tags to reflect the Question IDs in the "tools" menu. 

[[http://i.imgur.com/QHg9ayl.png | width=300px]]