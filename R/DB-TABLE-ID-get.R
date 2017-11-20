#' Get an element from the table
#'
#' @export
`[.jms.database.table.id` <- function(x,i=T,j=T) {
  if(!length(i)) i<-TRUE
  if(!length(j)) j<-TRUE
  ids=x$.table[,'id']
  iltz=i<0
  if(any(iltz)) i[iltz]<-which(ids%in%(i[iltz]*-1))*-1
  `[.jms.database.table`(x,i,j)
}
