Consider databases as environment?
Certainly need an environment per database object, not a global one

#' Create a database manager object
#'
#' This function is used to initialise a database manager
#' @param path Path to the database on disk
#' @param version Version number for the database schema
#' @inheritParams jms.database.internal
#' @return Database object
#' @export
jms.database <- function(path,name,blank,validate,version=1) {
  path_attr_name=paste0(name,'_database_path_')
  db_attr_name=paste0(name,"_database")
  mod_attr_name=paste0(name,"_database_modified")
  lock_attr_name=paste0(name,"_database_lockfile")
  DB_LOCK_TIMEOUT=10
  pathGetter <- function() {
    mget(x=path_attr_name,envir=jms_env,ifnotfound = NA)[[1]]
  }
  pathSetter <- function(newpath) {
    assign(x=path_attr_name, newpath, envir=jms_env)
  }
  lockGetter <- function() {
    mget(x=lock_attr_name,envir=jms_env,ifnotfound = NA)[[1]]
  }
  lockSetter <- function(newpath) {
    assign(x=lock_attr_name, newpath, envir=jms_env)
  }
  lock <- function() {
    path=lockGetter()
    if(is.na(path)) stop("Unable to obtain lock: Path is invalid")

    warning_shown=FALSE
    for(i in 1:DB_LOCK_TIMEOUT) {
      res=dir.create(path,showWarnings=FALSE)
      if(res==TRUE) return(invisible(TRUE))
      Sys.sleep(1)
      if(! warning_shown) {
        warning("Unable to obtain lock, retrying for ",DB_LOCK_TIMEOUT, " seconds.",immediate. = TRUE)
        warning_shown=TRUE
      }
    }
    stop("Timeout whilst waiting for lock")
  }
  unlock <- function() {
    path=lockGetter()
    if(is.na(path)) stop("Unable to release lock: Path is invalid")
    unlink(sprintf(path),recursive=TRUE)
    invisible(TRUE)
  }
  update_checker <- function() {
    path=pathGetter()
    mt=file.info(path)$mtime
    if(is.na(mt)) mt=.POSIXct(0)
    mget(x=mod_attr_name, envir=jms_env,ifnotfound=list(.POSIXct(0)))[[1]]<mt
  }
  setter <- function(df) {
    path=pathGetter()
    if(is.na(path)) return(invisible())
    assign(x=db_attr_name, df, envir=jms_env)
    lock()
    saveRDS(df,path)
    assign(x=mod_attr_name, file.info(path)$mtime , envir=jms_env)
    unlock()
    return(invisible())
  }

  blank_wrap <- function() {
    df=blank()
    attr(df,'db_version')<-version
    df
  }

  getter <- function() {
    path=pathGetter()
    if(is.na(path)) return(mget(x=db_attr_name,envir=jms_env,ifnotfound = list(blank_wrap()))[[1]])
    lock()
    if(update_checker()) {
      #reload file
      df=readRDS(path)
      assign(x=mod_attr_name, file.info(path)$mtime , envir=jms_env)
      assign(x=db_attr_name, df, envir=jms_env)
    } else {
      df=mget(x=db_attr_name,envir=jms_env,ifnotfound = list(blank_wrap()))[[1]]
    }
    unlock()
    attr(df,'name')<-name
    return(df)
  }

  clear <- function() setter(blank_wrap())
  add <- function(...,id=numeric()) {
    db=getter()
    next_id=max(0,db[,'id'])+1  #0 to account for empty table!
    if(!length(id)==1) id=next_id

    en=list(id=id,...)
    func_list=which(names(en) %in% names(formals(validate)))
    func_list=en[func_list]
    xargs=do.call(validate,func_list)

    new_index=which(db[,'id']==xargs$id)
    if(!length(new_index)==1) new_index=nrow(db)+1 #Index =!= id

    n=names(xargs)
    for(i in 1:length(n)) {
      value=xargs[n[[i]]][[1]]
      #can't replace an empty row with an empty row
      if(length(value)==0) value=NA
      db[[new_index, n[[i]]]] <- value
    }
    setter(db)
    return(invisible(new_index))
  }
  update <- function(id,...) add(...,id=id)
  remove <- function(ids) {
    db=getter()
    rows=which(db[,'id']%in%ids)
    if(length(row)==0) stop("Attempted to remove a ",name, " that doesn't exist")
    db=db[-rows,]
    if(nrow(db)) rownames(db)<-1:nrow(db)
    setter(db)
  }
  #Version is stored in database in case the schema changes
  #TODO: If so, copy the original file to <name>_V<version>.<ext>
  #Modify the database to adapt to the new schema using default (or empty) values if necessary
  version <- function() attr(getter(),'db_version')

  jms.database.internal(name,getter,setter,blank_wrap,validate,update_checker,pathGetter,pathSetter,lock,unlock,lockSetter,clear,add,update,remove,version)
}

#' Create a database manager object
#'
#' This function is used to initialise a database manager
#' @param name The name of the database & objects it contains (singular)
#' @param get Function to get the database
#' @param set Function to set the database
#' @param blank Function to get a blank dataframe
#' @param validate Function to validate changes to the database
#' @param update_checker Function that return TRUE if database needs refreshing from disk
#' @param lock Function to obtain lock on lockfile
#' @param unlock Function to release lock on lockfile
#' @param lockSetter Function to set path to lockfile
#' @param clear Function to clear the database
#' @param add Function to validate and add a row to the database
#' @param update Function to validate and update an existing row in the database
#' @param remove Function to remove one or more rows from the database
#' @param version Function to return the version number
#' @return Database object
#' @keywords internal
jms.database.internal <- function(name,get,set,blank,validate,update_checker,pathGetter,pathSetter,lock,unlock,lockSetter,clear,add,update,remove,version) {
  as.jms.database(list(
    name=name,
    get=get,
    set=set,
    blank=blank,
    validate=validate,
    update_checker=update_checker,
    path=pathGetter,
    setPath=pathSetter,
    lock=lock,
    unlock=unlock,
    setLockfile=lockSetter,
    clear=clear,
    add=add,
    update=update,
    remove=remove,
    version=version))
}
