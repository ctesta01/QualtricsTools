There are different usage guidelines and requirements depending on whether or not you will be using the Legacy or Insights Qualtrics platform. There are a lot of specific terms in this document, so please feel free to refer to the appendix included in this wiki.

- [Guidelines for Insights](https://github.com/ctesta01/QualtricsTools/wiki/Usage-Requirements/#guidelines-for-insights)
- [Guidelines for Legacy](https://github.com/ctesta01/QualtricsTools/wiki/Usage-Requirements/#guidelines-for-legacy)

We also have an [appendix of terms](https://github.com/ctesta01/QualtricsTools/wiki/Appendix-of-Qualtrics-Terms) to help clarify any confusing vocabulary used below. 

# Guidelines for Insights
When a user uploads Insights format data, the Question IDs and Import IDs are used to link the response columns to questions. Many of the guidelines and requirements for Legacy data do not apply to Insights generated data because this method is more consistent and accurate in linking responses to questions. 

#### Guidelines:
- Do not remove the header rows from the response set CSV
- Do not use the same Recode Values for different question options within the same question
- Do not use Data Export Tags or Recode Values including the string "TEXT" in them -- those columns will get processed as text appendices

#### Small Things to Keep in Mind
- Some characters, such as "-" and "#", will get converted to "_" characters in the CSV response set
- If you use negative Recode Values in matrix questions, those answers will be treated as NA-type answers and will be tabled separately


# Guidelines for Legacy
With Legacy data, there are no Import IDs available in the response set, so the method of linking responses to questions is more variable. The response column headers are constructed based on the Data Export Tag and Recode Values of a given question, and the formatting is dependent on the question type. As a result, response columns are linked to based on their Data Export Tags and Recode Values -- so there are more particular rules one must follow in order to avoid causing problems in the process of linking response columns to questions.

#### Guidelines:
- Do not remove the header rows from the response set CSV
- Do not repeat question Data Export Tags
- A response column name cannot start with the Data Export Tag or a Recode Value of another question -- it will be picked up by that question
- Do not use Data Export Tags or Recode Values including the string "TEXT" in them -- those columns will get processed as text appendices
- Do not use the same string as both a question's export tag and as a choice's Recode Value. 

#### Small Things to Keep in Mind
- Some characters, such as "-" and "#", will get converted to "_" characters in the CSV response set
- For certain question types, Legacy generated response columns' names only contain Recode Values without Data Export Tags
- If you use negative Recode Values in matrix questions, those answers will be treated as NA-type answers and will be tabled separately