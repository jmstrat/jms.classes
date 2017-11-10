#Database manager (we need some way to provide links between databases):
jms.database.manager <- function(directory) {
  self=new.env()
  self$managed_names<- character()
  self$managed_databases <- list()
  self$directory <- directory
  self$locktimeout=10

  self$path=paste0(self$directory,'/manager.jms.database.manager')

  self$init <- function() {
    if(file.exists(self$path))
      self$load_from_disk()
  }

  self$register_database <- function(database) {
    if(!inherits(database,'jms.database')) stop('Attempted to register a non database object')
    if(length(which(self$managed_names==database$name))) stop('A database with this name is already registered')
    self$managed_names=append(self$managed_names,database$name)
    self$managed_databases=append(self$managed_databases,list(database))
    #override setPath and setLockfile ???

    #set database path here
    #...

    self$save_to_disk()
    invisible()
  }
  self$unregister_database <- function(database) {
    if(!inherits(database,'jms.database')) stop('Attempted to unregister a non database object')
    index=which(self$managed_names==database$name)
    if(!length(index)) stop('A database with this name is not registered')
    self$managed_names=self$managed_names[-index]
    self$managed_databases=self$managed_databases[-index]
    self$save_to_disk()
    invisible()
  }
  self$get_database <- function(name) {
    self$load_from_disk()
    index=which(self$managed_names==name)
    if(!length(index)) stop('Database not found')
    self$managed_databases[[index]]
  }
  self$setPath <- function(path) {
    if(!dir.exists(path)) stop('Path does not correspond to a directory')
    self$directory=path
    for(database in self$managed_databases) {
      if(database$managed_path) {
        database$setPath(paste0(self$directory,'/',database$name,'.jms.database.table'))
        database$setLockfile(paste0(self$directory,'/lockfile.lock'))
      }
    }
    self$save_to_disk()
    invisible()
  }
  self$save_to_disk <- function() {
    self$lock()
    saveRDS(self$managed_databases,self$path)
    self$unlock()
  }
  self$load_from_disk <- function() {
    if(!length(directory)) stop('Database manager must have a directory')
    self$lock()
    self$managed_databases <- readRDS(self$path)
    self$unlock()
    self$managed_names <- vapply(self$managed_databases, function(x) x$name,"")
  }
  self$lock <- function() {
    warning_shown=FALSE
    for(i in 1:self$locktimeout) {
      res=dir.create(paste0(self$path,'.lock'),showWarnings=FALSE)
      if(res==TRUE) return(invisible(TRUE))
      Sys.sleep(1)
      if(! warning_shown) {
        warning("Unable to obtain lock, retrying for ",DB_LOCK_TIMEOUT, " seconds.",immediate. = TRUE)
        warning_shown=TRUE
      }
    }
    stop("Timeout whilst waiting for lock")
  }
  self$unlock <- function() {
    unlink(paste0(self$path,'.lock'),recursive=TRUE)
    invisible(TRUE)
  }

  self$init()
  self
}


#####
manager=jms.database.manager('/Users/Josh/Cambridge/PhD/Data/Cell_Database_TEMP')
cdb=Echem.Database::capacity_database
class(cdb)<-c('jms.database','list')
manager$register_database(cdb)
cdb$name='capacity2'
manager$register_database(cdb)

manager2=jms.database.manager('/Users/Josh/Cambridge/PhD/Data/Cell_Database_TEMP')
manager2$managed_names

When jms.database.object(...,register_database=TRUE)
is called it registers with the default manager

If set path is called on an individual database, its managed_path flag is set to FALSE
