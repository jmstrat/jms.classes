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
`[[<-.jms.database` <- function(x, name, value,.internal=FALSE) {
  if(startsWith(name,'.')) {NextMethod(); return(x)}
  index=which(x$.table_names==name)
  #Remove a table
  if(is.null(value)) {
    log.info('Removing table %s', name)
    if(!length(index)) return(x)
    x$.table_names=x$.table_names[-index]
    x$.tableModTimes=x$.tableModTimes[-index]
    x$.hasChanged=TRUE
    rm(name, envir = x)
  } else {
    if(!inherits(value,'jms.database.table')) stop('Attempted to add a non table object')
    tablePath=paste0(x$.path,'/',name,'.table')
    parent.env(value)<-x
    if(!length(index)) {
      log.info('Adding table %s', name)
      x$.table_names=append(x$.table_names,name)
      x$.tableModTimes=append(x$.tableModTimes,.POSIXct(0))
      log.info('Setting path for table %s to %s',name,tablePath)
      x$.tablePaths=append(x$.tablePaths,tablePath)
      value$.name<-name
      assign(name, value, envir = x)
      x$.hasChanged=TRUE
    }
    #Save the table
    if((!.internal)&&value$.hasChanged) {
      log.info('Saving table %s',name)
      table=get('.table',envir=value)
      make_lockfile(paste0(tablePath,'.lock'))
      saveRDS(table,tablePath)
      remove_lockfile(paste0(tablePath,'.lock'))
    }
  }
  if(!.internal) save(x)
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
