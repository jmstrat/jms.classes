#' Convert an r \code{expression} to \code{HTML}
#' @keywords internal
expressionToHTML <- function(x) {
  if(is.character(x)) return(x)
  x<-deparse(x,backtick=F,control=c())
  if(startsWith(x,'expression')) {
    x<-gsub('^expression\\((.*)\\)$','\\1',x)
    x<-gsub(' ','~',x)
  }
  x<-gsub(' ','',x)
  x<- gsub('\\*','',x)
  x<- gsub('~',' ',x)
  x<-gsub('^"','',x)
  x<-gsub('"$','',x)
  x<- gsub('([^\\])"','\\1',x)
  x<- gsub('([^\\])\\[([^]]*)\\]','\\1<sub>\\2</sub>',x)
  x<- gsub('([^\\])\\^([^[:space:]]*)','\\1<sup>\\2</sup>',x)
  x
}

#' Convert an r \code{expression} to a \code{character} string
#' @keywords internal
expressionToString <- function(x) {
  if(is.character(x)) return(x)
  x<-deparse(x,backtick=F,control=c())
  if(startsWith(x,'expression')) {
    x<-gsub('^expression\\((.*)\\)$','\\1',x)
    x<-gsub(' ','~',x)
  }
  x<-gsub(' ','',x)
  x<- gsub('\\*','',x)
  x<- gsub('~',' ',x)
  x<- gsub('([^\\])\\[|\\]|"|\\^','\\1',x)
  x
}
