# slmisc

miscellaneous functions to conduct projects in LeeLab

# Example:

library(devtools)

install_github("seonjoo/slmisc")

library(slmisc)

library(dplyr)

library(knitr)

data(mtcars)

varlist=c('mpg','disp','hp','drat','wt','gear','carb','vs')

desc_table(varlist, data=mtcars,groupvar=NULL)

desc_table(varlist, data=mtcars,groupvar="cyl")
