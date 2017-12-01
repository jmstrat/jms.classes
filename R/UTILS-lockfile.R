lockfile_counter = new.env()

#' Create a directory to act as a lockfile, error if the file already exists
#'
#' @param path Path to the lockfile
#' @param timeout How many times to attempt to obtain the lock
#' @param unique The lock may only be obtained once during this session
#'
#' @export
#' @rdname lockfile
make_lockfile <- function(path,timeout=10, unique=FALSE) {
  if(is.na(path)) stop("Unable to obtain lock: Path is invalid")
  pname=make.names(file.path(normalizePath(dirname(path),mustWork = FALSE),basename(path)))
  counter=lockfile_counter[[pname]]
  if(length(counter) && counter>0) {
    log.debug('lock counter: %s=%s',pname,counter)
    if(!unique) {
      lockfile_counter[[pname]]=counter+1
      return(invisible(TRUE))
    }
  } else {
    counter=0
    lockfile_counter[[pname]]=0
  }
  log.debug('lock counter: %s=%s',pname,counter)
  warning_shown=FALSE
  if(timeout>0) {
    for(i in 1:timeout) {
      res=dir.create(path,showWarnings=FALSE)
      if(res==TRUE) {
        lockfile_counter[[pname]]=counter+1
        return(invisible(TRUE))
      }
      if(! warning_shown) {
        warning("Unable to obtain lock, retrying for ",timeout, " seconds.",immediate. = TRUE)
        warning_shown=TRUE
      }
      Sys.sleep(1)
    }
  }
  stop("Timeout whilst waiting for lock")
}

#' @export
#' @rdname lockfile
remove_lockfile <- function(path) {
  if(is.na(path)) stop("Unable to release lock: Path is invalid")
  pname=make.names(file.path(normalizePath(dirname(path),mustWork = FALSE),basename(path)))
  counter=lockfile_counter[[pname]]
  log.debug('Using lock counter: %s',pname)
  if(!length(counter)) stop('Unable to release lock: lock was never obtained')
  log.debug('lock counter: %s=%s',pname,counter)
  if(counter>1) {
    lockfile_counter[[pname]]=counter-1
    return(invisible(TRUE))
  } else if(counter==1) {
    lockfile_counter[[pname]]=0
    unlink(sprintf(path),recursive=TRUE)
    return(invisible(TRUE))
  }
  stop('Unable to release lock: lock was never obtained')
}
