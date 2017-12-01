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
    x$.hash<-digest::digest(path, algo = "sha1", file = T)
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
  path=paste0(x$.path,'/database')
  if(is.null(x$.path) || (! file.exists(path))) {
    log.info('Not loading database: path is not set')
    return(invisible(x))
  }
  log.info('Preparing to load database')
  if(!is.stale(x)) {
    log.info('Database does not need loading')
    return(invisible(x))
  }
  lock(x)
  new_table_names<-readRDS(path)
  x$.hash<-digest::digest(path, algo = "sha1", file = T)
  log.info('Found tables: %s',paste0(new_table_names,collapse=','))
  for(name in new_table_names){
    if(name %in% x$.table_names) {
      log.info('Table %s already loaded',name)
      next() #We already have this table!
    }
    x$.table_names=append(x$.table_names,name)
    x$.tableHashes=append(x$.tableHashes,'') #Blank hash will force reload
  }
  x$.hasChanged=FALSE
  unlock(x)
  log.info('Database loaded')
  return(invisible(x))
}

