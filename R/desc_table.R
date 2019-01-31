#' Create descriptive table by group
#' Current version works in the html setting. We are working on extend it to the word document
#'
#' @param varlist One-dimensional predictor
#' @param data
#' @param groupvar
#' @return squareroot of matrix
#' @examples
#' data(mtcars)
#' mtcars = mtcars %>% mutate(cyl=factor(cyl),vs=factor(vs), am=factor(am))
#' varlist=c('mpg','disp','hp','drat','wt','gear','carb','vs')
#' desc_table(varlist, data=mtcars,groupvar=NULL)
#' desc_table(varlist, data=mtcars,groupvar="cyl")
#' @author Seonjoo Lee, \email{sl3670@cumc.columbia.edu}
#' @references TBA
#' @keywords highdimensional mediation L1penalization
#' @import dplyr
#' @import qwraps2
#' @import knitr
#' @export


desc_table<-function(varlist, data,groupvar=NULL){

  ## determine variable types
  summary1<-list()
  vartype=unlist(lapply(varlist, function(x)ifelse(is.factor(data[,x])|is.character(data[,x]), 'cat','cont' )))
  for (j in 1:length(varlist)){

    if (vartype[j]=='cont'){
      summary1[[varlist[j] ]]<-list()
      eval(parse(text=paste('summary1[["',varlist[j],'"]][["',varlist[j],'"]] = ~ qwraps2::mean_sd(',varlist[j],
                            ', na_rm = TRUE,show_n="never")',sep='')))
    }
    if (vartype[[j]]=='cat'){
      catlist=names(table(data[,varlist[j]]))
      if(sum(is.na(data[,varlist[j]]))>0){
        data[,varlist[j]]=as.factor(ifelse(is.na(data[,varlist[j]]), 'NA' ,data[,varlist[j]]))
        catlist=names(table(data[,varlist[j]]))
      }
      eval(parse(text=paste("summary1[['",varlist[j],"']] =list(",
                            paste0(
                              paste("'",varlist[j],':',catlist,"'=~perc_n2(",varlist[j]," =='",catlist,"',na_rm=TRUE,show_denom='never')",sep=''),
                              collapse=','),
                            ")",sep='')))
    }
  }


  whole<-summary_table(data, summary1)
  group<-c()
  if(is.null(groupvar)==FALSE){
    eval(parse(text=paste('group<-summary_table(dplyr::group_by(data,',groupvar,'), summary1)',sep='')))
  }

  both=cbind(whole,group)
  kable(both)
  #  return(both)

}


perc_n2<-function (x, digits = getOption("qwraps2_frmt_digits", 1), na_rm = FALSE,
                   show_denom = "ifNA", markup = getOption("qwraps2_markup",
                                                           "latex"))
{
  d <- sum(!is.na(x))
  n <- sum(x, na.rm = na_rm)
  p <- frmt(100 * n/d, digits)
  if (show_denom == "never") {
    rtn <- paste0(frmt(as.integer(n)), " (", p, ")")
  }
  else {
    if (show_denom == "always" | any(is.na(x))) {
      rtn <- paste0(frmt(as.integer(n)), " (", p,
                    "non-missing)")
    }
    else {
      rtn <- paste0(frmt(as.integer(n)), " (", p,
                    ")")
    }
  }
  if (markup == "latex") {
    rtn <- gsub("%", "\\\\%", rtn)
  }
  return(rtn)
}
