DB_LOCK_TIMEOUT=10
#' @export
lock.jms.database <- function(x) {
  log.info('Locking database')
  path=paste0(x$.path,'/lockfile.lock')
  if(is.na(path)) stop("Unable to obtain lock: Path is invalid")

  warning_shown=FALSE
  for(i in 1:DB_LOCK_TIMEOUT) {
    res=dir.create(path,showWarnings=FALSE)
    if(res==TRUE) return(invisible(TRUE))
    if(! warning_shown) {
      warning("Unable to obtain lock, retrying for ",DB_LOCK_TIMEOUT, " seconds.",immediate. = TRUE)
      warning_shown=TRUE
    }
    Sys.sleep(1)
  }
  stop("Timeout whilst waiting for lock")
}

#' @export
unlock.jms.database <- function(x) {
  log.info('Unlocking database')
  path=paste0(x$.path,'/lockfile.lock')
  if(is.na(path)) stop("Unable to release lock: Path is invalid")
  unlink(sprintf(path),recursive=TRUE)
  invisible(TRUE)
}
