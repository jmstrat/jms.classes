getHashForPath <- function(path) {
  digest::digest(path, algo="sha1", file=T)
}

getHashForTable <- function(database, name) {
  getHashForPath(database$.getPathForTable(name))
}

getHashForDatabase <- function(database) {
  getHashForPath(database$.getPathForDatabase())
}

checkHashForTable <- function(database, name) {
  database$.tableHashes[[name]] == database$.getHashForTable(name)
}

checkHashForDatabase <- function(database) {
  database$.hash == database$.getHashForDatabase()
}

updateHashForTable <- function(database, name) {
  dg <- database$.getHashForTable(name)
  database$.tableHashes[[name]] <- dg
  dg
}

updateHashForDatabase <- function(database) {
  dg <- database$.getHashForDatabase()
  database$.hash <- dg
  dg
}
