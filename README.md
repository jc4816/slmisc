# slmisc

miscellaneous functions to conduct projects in LeeLab

Current desc_table version only works with R-markdown, html setting. 

# Example:

library(devtools)

install_github("seonjoo/slmisc")

data(mtcars)

varlist=c('mpg','disp','hp','drat','wt','gear','carb','vs')

desc_table(varlist, data=mtcars,groupvar=NULL)

desc_table(varlist, data=mtcars,groupvar="cyl")
