# Renders all RMD files
#
# run with 
# Rscript make.r
library('rmarkdown')
rmarkdown::render('01.installing.qiime.rmd', output_format=c("html_document", "pdf_document"))
rmarkdown::render('02.loading.data.rmd', output_format=c("html_document", "pdf_document"))
rmarkdown::render('index.rmd', output_format=c("html_document"))
