current_database <- new.env()

#' Get or set the current project database.
#'
#' Get or set the current project database. This setting is persistent across sessions.
#'
#' @param db A \code{\link{jms.database}}
#' @export
#' @rdname project_database
project_database <- function() {
  if (is.null(current_database$db) && !is.null(config_dir)) {
    path <- file.path(config_dir, "project.database")
    if (file.exists(path)) {
      set_project_database(readRDS(path))
    } else {
      set_project_database(jms.database(NULL))
    }
  }

  database2reference(current_database$db)
}
#' @export
#' @rdname project_database
set_project_database <- function(db) {
  current_database$db <- db
  if (!is.null(config_dir)) {
    saveRDS(db, file.path(config_dir, "project.database"))
  }
}

# TODO: THIS SHOULD BE A GENERIC -- jms.database or character (path) or default (error)
