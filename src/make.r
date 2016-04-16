# Renders all RMD files
#
# USAGE:
#
# To make all RMD to html and pdf, run with 
# Rscript make.r
# 
# To make specific rmd files, run with
# Rscript make.r file1.rmd file2.rmd ...

library('rmarkdown')
args <- commandArgs(trailing=TRUE)

# if any RMD files were passed in the command line,
# process them

if(length(args) == 0){
    # if no args passed, find all rmd files
    args <- c(Sys.glob("*.rmd"), Sys.glob("*.RMD"))
}

for(arg in args){
    # Check that all args are rmd files
    if(!grepl(".rmd$",arg)){
        stop(paste('Non-RMD file passed on command line:',arg))
    }
    cat("Rendering HTML and PDF for input file",arg,"\n")
    rmarkdown::render(arg, output_format=c("html_document", "pdf_document"))
}
