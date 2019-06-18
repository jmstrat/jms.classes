config_dir <- NULL
config_db <- NULL
persistent_settings <- NULL

.onLoad <- function(libname, pkgname) {

  # Prevent excessively verbose debug logging by default
  jms.logging.threshold("INFO", "jms-database")
  jms.logging.threshold("INFO", "lock-files")

  config_dir <<- rappdirs::user_config_dir("jms.packages", "jms")
  dir.create(config_dir, showWarnings=FALSE, recursive=TRUE)
  # Load config db
  db_dir <- file.path(config_dir, "config-database")
  dir.create(db_dir, showWarnings=FALSE, recursive=TRUE)
  config_db <<- jms.database(db_dir)
  persistent_settings <<- tryCatch({
    config_db[["persistent_settings"]]
  }, error=function(e) {
    config_db[["persistent_settings"]] <- jms.database.table(key=character(), value=character())
    config_db[["persistent_settings"]]
  })
  # Load the project db
  project_database()

  # Register input handlers with shiny
  if (requireNamespace("shiny", quietly=TRUE)) {
    shiny::registerInputHandler("jms.matrix", shinyInputConvertToMatrix, force=TRUE)
  }

  return(invisible())
}

#' @export
get_persistent_setting <- function(key) {
  tryCatchST({
    get_key(key)
  }, error=function(e, st) {
    log.error("Error whilst getting persistent setting %s: %s\n%s", key, e, formatST(st))
    warning("Could not get setting ", key, immediate.=TRUE)
    NULL
  })
}

#' @export
set_persistent_setting <- function(key, value) {
  tryCatchST({
    set_key(key, value)
  }, error=function(e, st) {
    log.error("Error whilst setting persistent setting %s = %s: %s\n%s", key, value, e, formatST(st))
    warning("Could not set ", key, immediate.=TRUE)
    NULL
  })
}

get_key <- function(key, table=persistent_settings) {
  i <- which(key == table[, "key"])
  if (length(i)) {
    table[i, "value"]
  } else {
    NULL
  }
}

set_key <- function(key, value, table=persistent_settings) {
  i <- which(key == table[, "key"])
  if (length(i)) {
    table[i, "value"] <- value
  } else {
    table[nrow(table) + 1, ] <- list(key, value)
  }
}
