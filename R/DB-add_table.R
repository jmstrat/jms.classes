#' Add a table to a database
#'
#' This function is used to add a \code{\link{jms.database.table}} to a database
#' @param x The database
#' @param name The name of the table
#' @param value The table
#' @return Database object
#' @examples
#' database=jms.database(<path>)
#' database[['mytable']]<-jms.database.table(...)
#' @export
#' @rdname jms.database.add
`[[<-.jms.database` <- function(x, name, value) {
  if(startsWith(name,'.')) {NextMethod(); return(x)}
  index=which(x$.table_names==name)
  #Remove a table
  if(is.null(value)) {
    log.info('Removing table %s', name)
    if(!length(index)) return(x)
    x$.table_names=x$.table_names[-index]
    x$.hasChanged=TRUE
  } else {
    if(!inherits(value,'jms.database.table')) stop('Attempted to add a non table object')
    if(!length(index)) {
      log.info('Adding table %s', name)
      x$.hasChanged=TRUE
    } else {
      log.info('Updating table %s', name)
    }
    #Add a table
    x$.table_names=append(x$.table_names,name)
    tablePath=paste0(x$.path,'/',name,'.table')
    log.info('Setting path for table %s to %s',name,tablePath)
    attr(value,'.path')<-tablePath
    attr(value,'.lockfile')<-paste0(tablePath,'.lock')
    attr(value,'.name')<-name
    attr(value,'.database')<-x
    #Save the table
    lock(x)
    value<-save(value)
    unlock(x)
  }
  NextMethod()
  save(x)
  return(x)
}
#' @export
#' @rdname jms.database.add
`[<-.jms.database` <- function(x, name, value) {
  x[[name]] <- value
  return(x)
}
#' @export
#' @rdname jms.database.add
`$<-.jms.database` <- function(x, name, value) {
  x[[name]] <- value
  return(x)
}
#' @export
#' @rdname jms.database.add
`+.jms.database` <- function(e1, e2){
  #TODO: get name, call x[[name]] <- value
  stop()
}
#' @export
#' @rdname jms.database.add
`-.jms.database` <- function(e1, e2){
  #TODO: get name, call x[[name]] <- NULL
  stop()
}
