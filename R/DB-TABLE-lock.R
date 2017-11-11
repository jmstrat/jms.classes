#' @export
lock.jms.database.table <- function(x) {
  log.info('Locking table')
  make_lockfile(attr(x, '.lockfile'))
}

#' @export
unlock.jms.database.table <- function(x) {
  log.info('Unlocking table')
  remove_lockfile(attr(x, '.lockfile'))
}
