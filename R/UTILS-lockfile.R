#' Create a directory to act as a lockfile, error if the file already exists
#'
#' @param path Path to the lockfile
#' @param timeout How many times to attempt to obtain the lock
#'
#' @export
#' @rdname lockfile
make_lockfile <- function(path,timeout=10) {
  if(is.na(path)) stop("Unable to obtain lock: Path is invalid")

  warning_shown=FALSE
  for(i in 1:timeout) {
    res=dir.create(path,showWarnings=FALSE)
    if(res==TRUE) return(invisible(TRUE))
    if(! warning_shown) {
      warning("Unable to obtain lock, retrying for ",timeout, " seconds.",immediate. = TRUE)
      warning_shown=TRUE
    }
    Sys.sleep(1)
  }
  stop("Timeout whilst waiting for lock")
}

#' @export
#' @rdname lockfile
remove_lockfile <- function(path) {
  if(is.na(path)) stop("Unable to release lock: Path is invalid")
  unlink(sprintf(path),recursive=TRUE)
  invisible(TRUE)
}
