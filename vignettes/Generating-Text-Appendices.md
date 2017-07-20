To create text appendices in one step:
```R
make_text_appendices(qsf_path='C:/Users/oirera01/Desktop/Sample_Survey.qsf',
                     csv_path='C:/Users/oirera01/Desktop/Sample_Survey.csv',
                     headerrows=3, 
                     output_dir='C:/Users/oirera01/Desktop/Testing/',
                     filename='Text Appendices.docx')
```

Alternatively, you can load a survey into the environment and then 
process it after having a chance to edit the survey data in R. 
```R
# Use get_setup interactively to load survey data
get_setup()

# Make edits if desired

# Without arguments, make_text_appendices defaults to loading the survey data 
# from the global scope and defaults the filename to 'Text Appendices.docx'
make_text_appendices(output_dir='C:/Users/oirera01/Desktop/Testing/')
```

To generate text appendices in a more hands-on way, one first creates the text appendices as HTML with 
the `text_appendices_table` function. Then, the HTML contents is converted using 
`html_2_pandoc`. The following example makes use of the Survey Flow. 
If you would like to avoid using it, simply 
do not define `flow`, and do not pass it as an argument to `text_appendices_table`. 


```R
get_setup(
  qsf_path='C:/Users/oirera01/Desktop/Sample_Survey.qsf',
  csv_path='C:/Users/oirera01/Desktop/Sample_Survey.csv',
  headerrows=3
)

output_dir <- 'C:/Users/oirera01/Desktop/'

html_2_pandoc(
  html=c(blocks_header_to_html(blocks),
          text_appendices_table(blocks, original_first_rows, flow)), 
  file_name='sample survey text appendices.docx',
  output_dir=output_dir
)
```