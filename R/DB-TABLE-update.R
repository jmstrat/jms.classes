#' @export
`[<-.jms.database.table` <- function(x,i,j,value) {
  if(missing(i)) i=T
  if(missing(j)) j=T
  log.info('Updating {[%s],[%s]} with {%s}',paste0(i,collapse=','),paste0(j,collapse=','),paste0(value,collapse=','))
  #Validate the new value
  validator=x$.validator
  table=x$.table
  if(!is.null(validator) && !(i<0)) {
    log.info('Validating new values')
    n=colnames(table)
    names(n)<-n
    log.debug('Available columns: [%s]',paste0(n,collapse=','))
    tovalidate=n[j]
    log.debug('Columns supplied:  [%s]',paste0(tovalidate,collapse=','))
    if(any(is.na(tovalidate))) stop('Invalid options supplied')
    l=length(tovalidate)
    validate=matrix(value,nrow=length(value)/l,ncol=l)
    log.debug('New values matrix: %s',toString(validate))
    #Now we need to split a (potentially) 2D matrix into 1D vectors
    validatelist=split(validate, rep(1:ncol(validate), each = nrow(validate)))
    names(validatelist)<-tovalidate
    log.debug('Argument for validator: %s',paste(names(validatelist),validatelist,sep='=',collapse=','))
    value<-do.call(validator,as.list(validatelist))[j]
    log.debug('Values returned: [%s]',paste0(value,collapse=','))
    #Now we need to restore the original dimensions
    value<-matrix(unlist(value,use.names=FALSE),nrow=nrow(validate),ncol=ncol(validate))
    log.info('Validated update {[%s],[%s]} with {%s}',paste0(i,collapse=','),paste0(j,collapse=','),paste0(value,collapse=','))
  }
  df<-`[<-.data.frame`(table,i,j,value)
  #Mark the table as modified
  x$.hasChanged<-TRUE
  #Update rownames
  rownames(df)<-1:nrow(df)
  #Make the change
  x$.table<-df
  return(x)
}
#' @export
`[[<-.jms.database.table` <- function(x, ...) {
  `[<-`(x,...)
}
#' @export
`$<-.jms.database.table` <- function(x, name,value,...) {
  if(name=='.table') {
    assign(name, value, envir = x)
    save(x)
  }
  if(startsWith(name,'.')) {
    assign(name, value, envir = x)
    return(x)
  }
  stop('$ operator invalid for database objects')
}

