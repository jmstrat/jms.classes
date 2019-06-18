lockfile_counter <- new.env()

#' Create a directory to act as a lockfile, error if the file already exists
#'
#' @param path Path to the lockfile
#' @param timeout How many times to attempt to obtain the lock
#' @param unique The lock may only be obtained once during this session
#' @param force Remove the lock file and reset the counter to 0 even if the lock was never obtained
#'
#' @export
#' @rdname lockfile
make_lockfile <- function(path, timeout=10, unique=FALSE) {
  if (is.na(path)) stop("Unable to obtain lock: Path is invalid")
  pname <- make.names(cannonicalPath(path))
  counter <- lockfile_counter[[pname]]
  if (length(counter) && counter > 0) {
    log.debug("lock counter: %s=%s", pname, counter, ns='lock-files')
    if (!unique) {
      lockfile_counter[[pname]] <- counter + 1
      return(invisible(TRUE))
    }
  } else {
    counter <- 0
    lockfile_counter[[pname]] <- 0
  }
  log.debug("lock counter: %s=%s", pname, counter, ns='lock-files')
  warning_shown <- FALSE
  if (timeout > 0) {
    for (i in 1:timeout) {
      res <- dir.create(path, showWarnings=FALSE)
      if (res == TRUE) {
        lockfile_counter[[pname]] <- counter + 1
        return(invisible(TRUE))
      }
      if (!warning_shown) {
        warning("Unable to obtain lock, retrying for ", timeout, " seconds.", immediate.=TRUE)
        warning_shown <- TRUE
      }
      Sys.sleep(1)
    }
  }
  stop("Timeout whilst waiting for lock at: \"", path, "\" if this continues, close all R sessions and remove this directory.")
}

#' @export
#' @rdname lockfile
remove_lockfile <- function(path, force=FALSE) {
  if (is.na(path)) stop("Unable to release lock: Path is invalid")
  pname <- make.names(cannonicalPath(path))
  counter <- lockfile_counter[[pname]]
  log.debug("Using lock counter: %s", pname, ns='lock-files')

  if (force) {
    log.warn("Forcefully unlocking %s", path, ns='lock-files')
    warning("Forcefully removing lock file")
    lockfile_counter[[pname]] <- 0
    unlink(sprintf(path), recursive=TRUE)
    return(invisible(TRUE))
  }

  if (!length(counter)) stop("Unable to release lock: lock was never obtained")
  log.debug("lock counter: %s=%s", pname, counter, ns='lock-files')
  if (counter > 1) {
    lockfile_counter[[pname]] <- counter - 1
    return(invisible(TRUE))
  } else if (counter == 1) {
    lockfile_counter[[pname]] <- 0
    unlink(sprintf(path), recursive=TRUE)
    return(invisible(TRUE))
  }
  stop("Unable to release lock: lock was never obtained")
}
