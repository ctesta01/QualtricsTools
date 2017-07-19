This page is to show the results of some different features available in the QualtricsTools package. Those results can roughly be categorized into three categories: the output of the Shiny app, the output from reshaping the respondents, and the output from splitting the respondents. 

#### [Shiny App](https://github.com/ctesta01/QualtricsTools/wiki/The-Shiny-Web-Application)

The Shiny app allows a user to download four results documents. 

- results tables which detail the response rates for each question and its choices
- a question dictionary which details the question each response column corresponds to and its question types
- text appendices which table the respondents' responses to text entry questions
- display logic which lists for each question what survey logic, display logic, and survey flow options apply

Results Tables:
![results tables docx](http://i.imgur.com/VFrIHLy.png)

Question Dictionary:
![question dictionary csv](http://i.imgur.com/fFhwjog.png)

Text Appendices:
![text appendices docx](https://i.imgur.com/TyDuzOH.png)

Display Logic:
![display logic docx](https://i.imgur.com/XzAk0bi.png)

#### [Reshaping Respondents](https://github.com/ctesta01/QualtricsTools/wiki/Reshaping-Responses-for-Tableau)

The reshaping functions in the QualtricsTools package allow a user to convert the response data from the format Qualtrics provides it in, which gives little to no context to each answer, into long and lean data that gives a raw response and coded response for each respondent's answer to each question. The long and lean data format also allows for opportunity to merge in additional data, such as panel data and a question dictionary. 

Lean Responses: 
![lean responses worksheet](https://i.imgur.com/G4AatWL.png)

Question Dictionary: 
![question dictionary worksheet](https://i.imgur.com/ec1252D.png)

Panel Data:
![panel data](https://i.imgur.com/KyxPc0d.png)

#### [Splitting Respondents](https://github.com/ctesta01/QualtricsTools/wiki/Splitting-Respondents-into-Multiple-Reports)

The `splitting_respondents()` function allows a user to split the respondents into different subsets based on the values in a specified column. Using this, respondents can be split into different demographic groups, or different kinds of survey respondents. 

The headers in the images below reflect how the respondents are split by the `split_respondents()` function. 

![respondents who chose canteloupe](https://i.imgur.com/h12vRP7.png)

![respondents who chose pomagranite](https://i.imgur.com/BFctnwc.png)