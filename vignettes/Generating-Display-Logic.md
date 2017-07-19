Constructing a report of the display logic is pretty simple with QualtricsTools!

```R
get_setup(
  qsf_path='C:/Users/oirera01/Desktop/Sample_Survey.qsf',
  csv_path='C:/Users/oirera01/Desktop/Sample_Survey.csv',
  headerrows=3
)

output_dir = 'C:/Users/oirera01/Desktop/'
html_2_pandoc(
  html=c(blocks_header_to_html(blocks),
                tabelize_display_logic(blocks)), 
  file_name='sample survey display logic.docx',
  output_dir=output_dir
)
```