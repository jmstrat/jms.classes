#' @export
save.jms.database.table <- function(x,...) {
  log.info('Preparing to save table')
  if(!attr(x, '.hasChanged')) {
    log.info('Table does not need saving')
    return(x)
  }
  attr(x, '.hasChanged')<-FALSE
  path<-attr(x,'.path')
  if(is.null(path)) {
    warning('Database table does not have a valid path, cannot load data')
    return(x)
  }
  log.info('Saving table to %s',path)
  db<-attr(x,'.database')
  attr(x,'.database')<-NULL
  attr(x,'.path')<-NULL
  lock(x)
  saveRDS(x,path)
  unlock(x)
  attr(x,'.database')<-db
  attr(x,'.path')<-path
  attr(x,'.modTime')<-file.info(path)$mtime
  return(x)
}
#' @export
load.jms.database.table <- function(x,...) {
  log.info('Preparing to load table')
  if(!is.stale(x)) {
    log.info('Table does not need loading')
    return(x)
  }
  path<-attr(x,'.path')
  if(is.null(path)) {
    warning('Database table does not have a valid path, cannot load data')
    return(x)
  }
  lock(x)
  log.info('Loading table from %s',path)
  y<-readRDS(path)
  unlock(x)
  x<-y
  attr(x,'.path')<-path
  attr(x,'.modTime')<-file.info(path)$mtime
  attr(x, '.hasChanged')<-FALSE
  x
}
