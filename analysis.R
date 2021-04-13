
knitr::knit('ms/manuscript.Rnw', 'ms/manuscript.tex')

# need to install tinytex software if have not before
tinytex::install_tinytex()
tinytex::pdflatex('ms/manuscript.tex', 
                  pdf_file = 'ms/manuscript.pdf')

