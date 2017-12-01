#' @export
lock.jms.database <- function(x) {
  log.info('Locking database')
  make_lockfile(x$.lockfile)
}

#' @export
unlock.jms.database <- function(x) {
  log.info('Unlocking database')
  remove_lockfile(x$.lockfile)
}
