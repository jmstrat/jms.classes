#' Convert an r \code{expression} to \code{HTML}
#' @keywords internal
expressionToHTML <- function(x) {
  if(is.character(x)) return(x)
  x<-deparse(x,backtick=F,control=c())
  if(startsWith(x,'expression')) {
    x<-gsub('^expression\\((.*)\\)$','\\1',x)
    x<-gsub(' ','~',x)
  }

  comps = strsplit(x, '"')[[1]]
  unquoted = comps[c(T,F)]
  if(length(comps) > 1) {
    quoted = comps[c(F,T)]
  } else {
    quoted = c()
  }

  keys = names(HTML_CHARACTER_MAP)
  for(i in seq_along(HTML_CHARACTER_MAP)) {
    unquoted <- gsub(keys[[i]], HTML_CHARACTER_MAP[[i]], unquoted)
  }

  idx <- order(c(seq_along(unquoted), seq_along(quoted)))
  x <- paste0(unlist(c(unquoted,quoted))[idx], collapse='')

  x <- gsub('([^\\]?)\\[([^]]*)\\]','\\1<sub>\\2</sub>',x)
  x <- gsub('([^\\]?)\\^([^[:space:]]*)','\\1<sup>\\2</sup>',x)

  x<-gsub(' ','',x)
  x<- gsub('\\*','',x)
  x<- gsub('~',' ',x)
  x<-gsub('^"','',x)
  x<-gsub('"$','',x)
  htmltools::HTML(x)
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
