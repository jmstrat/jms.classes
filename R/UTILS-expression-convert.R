#' Convert an r \code{expression} to \code{HTML}
#' @keywords internal
expressionToHTML <- function(x) {
  x<-deparse(x)
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
  x<-deparse(x)
  x<-gsub(' ','',x)
  x<- gsub('\\*','',x)
  x<- gsub('~',' ',x)
  x<- gsub('([^\\])\\[|\\]|"|\\^','\\1',x)
  x
}
