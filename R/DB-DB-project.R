current_database <- new.env()
#current_database$db<-jms.database()

#' Set or get the current project database
#'
#' @export
#' @rdname project_database
project_database <- function() {
  current_database$db
}
#' @export
#' @rdname project_database
set_project_database <- function(db) {
  current_database$db<-db
}
