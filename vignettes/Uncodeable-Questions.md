If you're worried about which questions didn't work, and curious as to why, that information is easily obtained. 

First, please understand that the QualtricsTools project is currently limited to handling Text Entry, Single Answer Multiple Choice, Multiple Answer Multiple Choice, Single Answer Likert Matrix, and Multiple Answer Likert Matrix questions. Others are currently not supported, but perhaps in the future that will change. So please, do not be surprised when other types of questions are not automatically processed by QualtricsTools. 

If you'd like to see the details of the questions that were not automatically processed by the QualtricsTools, it's only one command away. After you `get_setup()` with your survey, run `uncodeable_question_dictionary(blocks)`.
![uncodeable_question_dictionary viewed in Rstudio](https://i.imgur.com/MPYNL73.png)