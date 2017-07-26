---
title: An Introduction to Recode Values in Qualtrics
---

# Using Recode Values in Qualtrics

Recode Values are used for different purposes depending on the question
type in the Qualtrics platform. Below is a quick reference to what kinds
of variables are available to be defined by the user in a given question
type, and where they appear in the response set. The Recode Values are
always numeric variables, and Question Export Tags can be set as
strings. On the following pages are short explanations for each question
type with some screenshots of the recode values menu in use and the
output the example creates.

- [Multiple Choice Single Answer](https://gist.github.com/ctesta01/9763dc35eee5301feb30c4fbfce6d2a0#multiple-choice-single-answer-questions-radio-buttons)
- [Multiple Choice Multiple Answer](https://gist.github.com/ctesta01/9763dc35eee5301feb30c4fbfce6d2a0#multiple-choice-multiple-answer-questions-check-boxes)
- [Matrix Single Answer](https://gist.github.com/ctesta01/9763dc35eee5301feb30c4fbfce6d2a0#likert-matrix-single-answer-questions)
- [Matrix Multiple Answer](https://gist.github.com/ctesta01/9763dc35eee5301feb30c4fbfce6d2a0#likert-matrix-multiple-answer-questions)

<table>
  <tr>
    <th>Question Type</th>
    <th>Variables Available for Use</th>
    <th>Where they Appear</th>
  </tr>
  <tr>
    <td>Multiple Choice Single Answer</td>
    <td>Recode Values</td>
    <td>Variable Responses</td>
  </tr>
  <tr>
    <td>Multiple Choice Multiple Answer</td>
    <td>Recode Values</td>
    <td>Response Column Names</td>
  </tr>
  <tr>
    <td rowspan="2">Matrix Single Answer</td>
    <td>Recode Values</td>
    <td>Variable Responses</td>
  </tr>
  <tr>
    <td>Question Export Tags</td>
    <td>Response Column Names</td>
  </tr>
  <tr>
    <td rowspan="2">Matrix Multiple Answer</td>
    <td>Recode Values</td>
    <td rowspan="2">Response Column Names</td>
  </tr>
  <tr>
    <td>Question Export Tags</td>
  </tr>
</table>
<br>
<br>


### Multiple Choice Single Answer Questions (Radio Buttons)

In MCSA questions, the Recode Values are numerical values that will be
used to assign variable values to each answer choice.

![](http://i.imgur.com/d1yYwCj.png)

![](http://i.imgur.com/Put08IR.png)
<br> 
<br>

### Multiple Choice Multiple Answer Questions (Check Boxes)

In MCMA questions, the Recode Values are numerical values that will be
appended to the data export tag (in this case "q3_volunteer") to label
each response column.

![](http://i.imgur.com/Q1AGzns.png)

![](http://i.imgur.com/kXx13oa.png)
<br> 
<br>

### Likert Matrix Single Answer Questions

There are two useful sections under the Recode Values menu for matrix
questions: the Recode Values and the Question Export Tags. The Recode
Values in a single answer matrix question are used to code the responses
and the Question Export Tags are used (by appending them to the data
export tag, in this case "q4_rank") to label the response columns.

![](http://i.imgur.com/FmVYnF9.png)

![](http://i.imgur.com/2jxHkan.png)
<br> 
<br>


### Likert Matrix Multiple Answer Questions

In multiple answer matrix questions, both the Recode Values and the
Question Export Tags (in combination with the data export tag) are used
to label the response columns.

![](http://i.imgur.com/7fwPDli.png)

![](http://i.imgur.com/HsrLRhe.png)
