#' @export
`[<-.jms.database.table` <- function(x, ...) {
  #Validate the new value
  log.warn('Table data is not yet validated')
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
  `[<-`(x,...)
}

