#' @export
save.jms.database <- function(x,...) {
  if(is.null(x$.path)) {
    log.info('Not saving database: path is not set')
    return(x)
  }
  log.info('Preparing to save database')
  if(x$.hasChanged) {
    path=paste0(x$.path,'/database')
    lock(x)
    log.info('Saving database to %s',path)
    saveRDS(x$.table_names,path)
    x$.modTime=file.info(path)$mtime
    unlock(x)
    x$.hasChanged=FALSE
  } else {
    log.info('Database does not need saving')
  }
  #Tables are saved when they are added to the database
  return(x)
}

#' @export
load.jms.database <- function(x,...) {
  if(is.null(x$.path)) {
    log.info('Not loading database: path is not set')
    return(invisible(x))
  }
  log.info('Preparing to load database')
  if(!is.stale(x)) {
    log.info('Database does not need loading')
    return(invisible(x))
  }
  lock(x)
  new_table_names<-readRDS(paste0(x$.path,'/database'))
  x$.modTime<-file.info(x$.path)$mtime
  log.info('Found tables: %s',paste0(new_table_names,collapse=','))
  for(name in new_table_names){
    if(name %in% x$.table_names && !is.stale(x[[name,.internal=TRUE]])) {
      log.info('Table %s already loaded',name)
      next() #We already have this table!
    }
    log.info('Creating table: %s',name)
    #We need to load this table
    skeleton=jms.database.table()
    attr(skeleton,'.path')<-paste0(x$.path,'/',name,'.table')
    attr(skeleton,'.lockfile')<-paste0(x$.path,'/',name,'.table.lock')
    x[[name,.internal=TRUE]]<-skeleton
    #Tables are loaded when they are got from the database
  }
  x$.hasChanged=FALSE
  unlock(x)
  log.info('Database loaded')
  return(invisible(x))
}

