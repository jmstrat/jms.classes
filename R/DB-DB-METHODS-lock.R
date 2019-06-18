lockTable <- function(database, name) {
  path <- database$.getPathForTable(name)
  make_lockfile(paste0(path, ".lock"))
}

unlockTable <- function(database, name, force=FALSE) {
  path <- database$.getPathForTable(name)
  remove_lockfile(paste0(path, ".lock"), force=force)
}

lockDatabase <- function(database) {
  path <- database$.getPathForDatabase()
  make_lockfile(paste0(path, ".lock"))
}

unlockDatabase <- function(database, force=FALSE) {
  path <- database$.getPathForDatabase()
  remove_lockfile(paste0(path, ".lock"), force=force)
}
