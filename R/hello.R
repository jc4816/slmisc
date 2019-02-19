#' Create descriptive table by group
#' Current version works in the html setting. We are working on extend it to the word document
#'
#' @param varlist One-dimensional predictor
#' @param data Dataset to be analyzed.
#' @param groupvar Group variable name. If not specified, descriptive stats for all subjects are reported.
#' @param total (default=TRUE). Valid only if groupvar is not NULL. total=FALSE if a user does not want to print stats for all subjects.
#' @param group.comp (default=TRUE) Add p-values of group comparison. F test and Chi-square test are used for continuouis and categorail variables, respectively.
#' @return squareroot of matrix
#' @examples
#' data(mtcars)
#' mtcars = mtcars %>% mutate(cyl=factor(cyl),vs=factor(vs), am=factor(am))
#' varlist=c('mpg','disp','hp','drat','wt','gear','carb','vs')
#' desc_table(varlist, data=mtcars,groupvar=NULL)
#' desc_table(varlist, data=mtcars,groupvar="cyl")
#' desc_table(varlist, data=mtcars,groupvar="cyl",total=FALSE)
#' @author Seonjoo Lee, \email{sl3670@cumc.columbia.edu}
#' @references TBA
#' @keywords highdimensional mediation L1penalization
#' @import dplyr
#' @import qwraps2
#' @import knitr
#' @export


hello<-function(){
  print('hello')
}
