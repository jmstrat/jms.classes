getPathForTable <- function(database, name) {
  dbPath <- database$.path
  if (!length(dbPath)) stop("Path is undefined")
  paste0(dbPath, "/", name, ".table")
}

getPathForDatabase <- function(database) {
  dbPath <- database$.path
  if (!length(dbPath)) stop("Path is undefined")
  paste0(dbPath, "/database")
}
