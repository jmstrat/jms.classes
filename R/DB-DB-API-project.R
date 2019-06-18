current_database <- new.env()

#' Get or set the current project database.
#'
#' Get or set the current project database. This setting is persistent across sessions.
#'
#' @param x A \code{\link{jms.database}} or the path to a database
#' @export
#' @rdname project_database
project_database <- function() {
  if (is.null(current_database$db) && !is.null(config_dir)) {
    path <- file.path(config_dir, "project.database")
    if (file.exists(path)) {
      log.info("Loading project database")
      set_project_database(jms.database(readRDS(path)))
    } else {
      log.info("Creating default (memory only) project database")
      set_project_database(jms.database(NULL))
    }
  }

  database2reference(current_database$db)
}
#' @export
#' @rdname project_database
set_project_database <- function(x) UseMethod("set_project_database")

set_project_database.default <- function(x) {
  stop("Unable to set project database to ", x)
}

set_project_database.jms.database <- function(x) {
  current_database$db <- x
  if (!is.null(config_dir)) {
    saveRDS(x$.path, file.path(config_dir, "project.database"))
  }
}

set_project_database.character <- function(x) {
  set_project_database(jms.database(x))
}
