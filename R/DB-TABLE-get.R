#' Get an element from the table
#'
#' @export
`[.jms.database.table` <- function(x,i=T,j=T) {
  if(!length(i)) i<-TRUE
  if(!length(j)) j<-TRUE
  iltz=i<0
  liltz=length(i[iltz])
  jltz=j<0
  ljltz=length(j[jltz])
  if(liltz && ljltz) {
    log.info('Deleting row%s %s and column%s %s',if(liltz==1) '' else 's',paste0(i[iltz]*-1,collapse=','), if(ljltz==1) '' else 's', paste0(j[jltz]*-1,collapse=','))
  } else if(liltz) {
    log.info('Deleting row%s %s',if(liltz==1) '' else 's', paste0(i[iltz]*-1,collapse=','))
  } else if(ljltz) {
    log.info('Deleting column%s %s',if(ljltz==1) '' else 's', paste0(j[jltz]*-1,collapse=','))
  }
  table<-x$.table
  if(liltz || ljltz) {
    x$.hasChanged<-TRUE
    if(liltz) {
      table<-`[.data.frame`(table,i[iltz],T)
      rownames(table)<-1:nrow(table)
    }
    if(ljltz) table<-`[.data.frame`(table,T,j[jltz])
    x$.table<-table
  }
  i <- i[!iltz]
  j <- j[!jltz]
  if(!length(i)) i<-TRUE
  if(!length(j)) j<-TRUE
  log.info('Getting {[%s],[%s]}',paste0(i,collapse=','),paste0(j,collapse=','))
  return(`[.data.frame`(table,i,j))
}
#' @export
`[[.jms.database.table`<-function(x,i=T,j=T) {
  stop('[[ subsetting not supported for objects of this type. Use [ instead.')
}
#' @export
`$.jms.database.table`<-function(x,i=T,j=T) {
  if(i=='.table')  {
    return(get(i,envir=load(x)))
  }
  if(startsWith(i,'.')) return(get(i,envir=x))
  stop('$ subsetting not supported for objects of this type. Use [ instead.')
}
