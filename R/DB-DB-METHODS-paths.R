getPathForTable <- function(database, name) {
  dbPath <- database$.path
  if (!length(dbPath)) stop("Path is undefined")
  name <- paste0(name, ".table")
  file.path(dbPath, name)
}

getPathForDatabase <- function(database) {
  dbPath <- database$.path
  if (!length(dbPath)) stop("Path is undefined")
  file.path(dbPath, "database")
}
