config_dir <- NULL
config_db <- NULL

.onLoad <- function(libname, pkgname) {
  init_log <- Sys.getenv("JMS_INSTALL_LOG")
  if (init_log == "1") {
    jms.enable.logging("DEBUG")
  } else {
    # Prevent excessively verbose debug logging by default
    jms.logging.threshold("INFO", "jms-database")
    jms.logging.threshold("INFO", "lock-files")
  }

  log.info("Creating directory to store persistent configuration")
  config_dir <<- rappdirs::user_config_dir("jms.packages", "jms")
  dir.create(config_dir, showWarnings=FALSE, recursive=TRUE)
  log.debug("Created configuration directory at %s", config_dir)

  # Load config db
  log.info("Loading configuration database")
  db_dir <- file.path(config_dir, "config-database")
  dir.create(db_dir, showWarnings=FALSE, recursive=TRUE)

  # Note that the database won't actually load at this point
  # It will actually load when we try to do anything with it
  # e.g. by calling persistent_settings_table()
  # This simplifies startup (and installation)
  config_db <<- jms.database(db_dir)

  # Load the project db
  # Again the database won't actually really load at this point
  project_database()

  # Register input handlers with shiny
  if (requireNamespace("shiny", quietly=TRUE)) {
    log.info("Registering shiny input handlers")
    shiny::registerInputHandler("jms.matrix", shinyInputConvertToMatrix, force=TRUE)
  }

  return(invisible())
}

persistent_settings_table <- function(database=config_db) {
  # Will raise for any error other than table_not_found
  tryCatch({
    database[["persistent_settings"]]
  }, table_not_found=function(e) {
    log.info("Initialising persistent settings table")
    database[["persistent_settings"]] <- jms.database.table(key=character(), value=character())
    database[["persistent_settings"]]
  })
}

#' @export
get_persistent_setting <- function(key) {
  tryCatchST({
    get_key(key, persistent_settings_table())
  }, error=function(e, st) {
    log.error("Error whilst getting persistent setting %s: %s\n%s", key, e, formatST(st))
    warning("Could not get setting ", key, immediate.=TRUE)
    NULL
  })
}

#' @export
set_persistent_setting <- function(key, value) {
  tryCatchST({
    set_key(key, value, persistent_settings_table())
  }, error=function(e, st) {
    log.error("Error whilst setting persistent setting %s = %s: %s\n%s", key, value, e, formatST(st))
    warning("Could not set ", key, immediate.=TRUE)
    NULL
  })
}

get_key <- function(key, table) {
  i <- which(key == table[, "key"])
  if (length(i)) {
    table[i, "value"]
  } else {
    NULL
  }
}

set_key <- function(key, value, table) {
  i <- which(key == table[, "key"])
  if (length(i)) {
    table[i, "value"] <- value
  } else {
    table[nrow(table) + 1, ] <- list(key, value)
  }
}

#' Create a system dependant configuration directory for use by a package
#' @export
get_configuration_directory <- function(package) {
  dir <- file.path(config_dir, package)
  dir.create(dir, showWarnings=FALSE, recursive=TRUE)
  log.debug("Created configuration directory for %s at %s", package, config_dir)
  dir
}
