#' @export
`[<-.jms.database.table` <- function(x,i,j,value) {
  if(missing(i)) i=T
  if(missing(j)) j=T
  tt<<-value
  log.info('Updating {[%s],[%s]} with {%s}',paste0(i,collapse=','),paste0(j,collapse=','),paste0(value,collapse=','))
  #Validate the new value
  validator=attr(x,'.validator')
  if(!is.null(validator)) {
    log.info('Validating new values')
    n=colnames(x)
    names(n)<-n
    log.debug('Columns: [%s]',paste0(n,collapse=','))
    tovalidate=n[j]
    log.debug('Columns supplied: [%s]',paste0(tovalidate,collapse=','))
    if(any(is.na(tovalidate))) stop('Invalid options supplied')
    validate=as.matrix(value)
    #Now we need to split a (potentially) 2D matrix into 1D vectors
    validatelist=split(validate, rep(1:ncol(validate), each = nrow(validate)))
    names(validatelist)<-tovalidate
    value<-do.call(validator,as.list(validatelist))[j]
    log.debug('Values returned: [%s]',paste0(value,collapse=','))
    #Now we need to restore the original dimensions
    value<-matrix(unlist(value),nrow=nrow(validate),ncol=ncol(validate))
    log.info('Validated update {[%s],[%s]} with {%s}',paste0(i,collapse=','),paste0(j,collapse=','),paste0(value,collapse=','))
  }
  #Make the change
  x<-NextMethod()
  #Mark the table as modified
  attr(x,'.hasChanged')<-TRUE
  #Add the table to the database
  attr(x,'.database')[[attr(x,'.name')]]<-x
  return(x)
}
#' @export
`[[<-.jms.database.table` <- function(x, ...) {
  `[<-`(x,...)
}
#' @export
`$<-.jms.database.table` <- function(x, ...) {
  stop('$ operator invalid for database objects')
}

