current_database <- new.env()

#' Set or get the current project database
#'
#' @export
#' @rdname project_database
project_database <- function() {
  current_database$db
}
#' @export
#' @rdname project_database
setproject_database <- function(db) {
  current_database$db<-db
}
