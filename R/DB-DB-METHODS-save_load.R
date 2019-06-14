writeDBOBJToFile <- function(obj, path) saveRDS(obj, path)
readDBOBJFromFile <- function(path) readRDS(path)

saveTable <- function(database, name) {
  table <- get0(name, envir=database)
  if (is.null(database$.path)) {
    return(table)
  }
  path <- database$.getPathForTable(name)
  parent.env(table) <- emptyenv()
  database$.lockTable(name)
  log.debug("Writing table %s to %s", name, path)
  writeDBOBJToFile(table, path)
  database$.updateHashForTable(name)
  database$.unlockTable(name)
  parent.env(table) <- database
  table
}

loadTable <- function(database, name) {
  path <- database$.getPathForTable(name)
  if (!file.exists(path)) stop("Unable to load table ", name)
  current_table <- get0(name, envir=database)
  database$.lockTable(name)
  log.debug("Reading table from %s", path)
  new_table <- readDBOBJFromFile(path)
  if (is.null(current_table)) {
    parent.env(new_table) <- database
    new_table$.name <- name
    assign(name, new_table, envir=database)
  } else {
    # We shouldn't replace the table environement, just the data
    for (n in ls(new_table, all.names=TRUE)) assign(n, get(n, new_table), current_table)
    new_table <- current_table
  }
  database$.updateHashForTable(name)
  database$.unlockTable(name)
  new_table
}

saveDatabase <- function(database) {
  if (is.null(database$.path)) {
    log.info("Not saving database: path is not set")
    return(database)
  }
  log.debug("Preparing to save database")
  if (database$.hasChanged) {
    path <- database$.getPathForDatabase()
    database$.lockDatabase()
    log.info("Saving database to %s", path)
    writeDBOBJToFile(database$.table_names, path)
    database$.updateHashForDatabase()
    database$.unlockDatabase()
    database$.hasChanged <- FALSE
  } else {
    log.debug("Database does not need saving")
  }
  # Tables are saved when they are added to the database
  return(database)
}

loadDatabase <- function(database) {
  if (is.null(database$.path)) {
    log.info("Not loading database: path is not set")
    return(invisible(database))
  }
  path <- database$.getPathForDatabase()
  if (!file.exists(path)) {
    log.warn("Not loading database: file does not exist")
    return(invisible(database))
  }
  log.debug("Preparing to load database")
  if (!is.stale(database)) {
    log.debug("Database does not need loading")
    return(invisible(database))
  }
  database$.lockDatabase()
  new_table_names <- readDBOBJFromFile(path)
  database$.updateHashForDatabase()
  log.info("Found tables: %s", paste0(new_table_names, collapse=","))
  for (name in new_table_names) {
    if (name %in% database$.table_names) {
      log.info("Table %s already loaded", name)
      next() # We already have this table!
    }
    database$.table_names <- append(database$.table_names, name)
    database$.tableHashes[[name]] <- "" # Blank hash will force reload
  }
  database$.hasChanged <- FALSE
  database$.unlockDatabase()
  log.info("Database loaded")
  return(invisible(database))
}
