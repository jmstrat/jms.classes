addTable <- function(database, name, table) {
  if(!inherits(table,'jms.database.table')) stop('Attempted to add a non table object')
  if(!name %in% database$.table_names) {
    log.info('Adding table %s', name)
    database$.table_names=append(database$.table_names,name)
    database$.tableHashes[[name]]=''
    database$.hasChanged=TRUE
  }
  #Add the table
  parent.env(table)<-database
  table$.name<-name
  assign(name, table, envir = database)
  #Save the table
  if(table$.hasChanged) {
    log.info('Saving table %s',name)
    database$.saveTable(name)
  }
  table
}

getTable <- function(database, name) {
  if(!name %in% database$.table_names) stop('Database table "',name,'" was not found')
  log.info('Getting table %s',name)
  if(!is.null(database$.path)) {
    if(!(database$.checkHashForTable(name))) {
      log.debug('Reloading table %s',name)
      table<-database$.loadTable(name)
    } else {
      log.debug('Table does not need reloading')
      table=get0(name, envir = database)
    }
  } else {
    log.debug('Table is not stored on disk')
    table=get0(name, envir = database)
  }
  table
}

removeTable <- function(database, name) {
  index=which(database$.table_names==name)
  if(!length(index)) return(database)
  log.info('Removing table %s', name)
  database$.table_names=database$.table_names[-index]
  database$.tableHashes[[name]]<-NULL
  database$.hasChanged=TRUE
  rm(name, envir = database)
}
