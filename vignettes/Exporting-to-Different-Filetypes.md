The web-app as well as the commandline package functions allow users to render reports in a variety of file-formats, as supported by [Pandoc](http://pandoc.org/), a universal document converter. The `html_2_pandoc` function in my package converts HTML, as constructed by the QualtricsTools app or functions, and renders it to files. Typically these are docx files, and this is the default behavior. However, occasionally a report is so large that it cannot successfully be rendered to docx directly, or for other reasons other filetypes are preferred. 

Not only does `html_2_pandoc` support many different filetypes, but since `make_results_tables` and `make_text_appendices` are built on top of `html_2_pandoc`, they also automatically support the filetypes Pandoc supports. 

```R
get_setup()
make_results_tables(output_dir='C:/Example/Path/Here', filename='Results Tables.txt')
make_results_tables(output_dir='C:/Example/Path/Here', filename='Results Tables.xls')
make_results_tables(output_dir='C:/Example/Path/Here', filename='Results Tables.html')
```

![TXT](http://i.imgur.com/WwYISE9.png)

![XLS](http://i.imgur.com/bjFd7wI.png)

![HTML](http://i.imgur.com/jcIFirR.png)