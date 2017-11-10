#' @export
`[<-.jms.database.table` <- function(x,i,j,value) {
  log.info('Updating [%s,%s] with {%s}',i,j,value)
  #Validate the new value
  validator=attr(x,'.validator')
  if(!is.null(validator)) {
    log.info('Validating new values')
    n=colnames(x)
    names(n)<-n
    log.debug('Columns: [%s]',paste0(n,collapse=','))
    tovalidate=n[j]
    log.debug('Values supplied: [%s]',paste0(tovalidate,collapse=','))
    if(any(is.na(tovalidate))) stop('Invalid options supplied')
    validate=value
    names(validate)<-tovalidate
    value<-do.call(validator,as.list(validate))[j]
    log.info('Validated update [%s,%s] with {%s}',i,j,value)
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

