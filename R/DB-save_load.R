#' @export
save.jms.database <- function(x,...) {
  log.info('Preparing to save database')
  lock(x)
  if(x$.hasChanged) {
    path=paste0(x$.path,'/database')
    log.info('Saving database to %s',path)
    saveRDS(x$.table_names,path)
    x$.modTime=file.info(path)$mtime
    x$.hasChanged=FALSE
  } else {
    log.info('Database does not need saving')
  }
  #Tables are saved when they are added to the database
  unlock(x)
  return(x)
}

#' @export
load.jms.database <- function(x,...) {
  log.info('Preparing to load database')
  if(!is.stale(x)) {
    log.info('Database does not need loading')
    return(invisible(x))
  }
  lock(x)
  new_table_names<-readRDS(paste0(x$.path,'/database'))
  log.info('Found tables: %s',new_table_names)
  for(name in new_table_names){
    if(name %in% x$.table_names && !is.stale(x[[name,.internal=TRUE]])) {
      log.info('Table %s already loaded',name)
      next() #We already have this table!
    }
    log.info('Loading table: %s',name)
    #We need to load this table
    delayedAssign('table', {
      table=jms.database.table()
      attr(table,'.path')<-paste0(x$.path,'/',name,'.table')
      table<-load(table)
      table})
    x[[name]]<-table
  }
  x$.hasChanged=FALSE
  unlock(x)
  log.info('Database loaded')
  return(invisible(x))
}

