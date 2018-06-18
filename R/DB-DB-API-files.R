#' Validate entries to the files database
#' @inheritParams update_files
#' @inheritParams add_files
#' @keywords internal
files_db_validate <- function(path=character()) {
  path=assert_file(path,"Invalid file path supplied")
  list(path=path)
}

#' Create an empty dataframe to function as the files database
#'
#' @keywords internal
.blank_files_db<- function() {jms.database.table.id(path=character(),validator=files_db_validate)}

#' Create an empty files database to replace the existing one
#'
#' This function is used to initialise a database to hold details of file paths
#' @export
clear_files_database <- function() {
  ft<-.blank_files_db()
  db<-project_database()
  db[['filePaths']]<-ft
  ft
}

#' @export
files_table <- function() {
  tryCatch({
    project_database()[['filePaths']]
  }, error = function(e) {
    print(e)
    clear_files_database()
  })
}

#' Add a file to the database
#'
#' This function is used to add files to the file paths database
#' @param path path to the file
#' @return IDs of the added files in the database
#' @export
add_files <- function(path=character()) {
  if(!length(path)) return(numeric())
  ft<-files_table()
  current_ids=id_for_files(path)
  for(file in path[is.na(current_ids)]) {
    if(!length(file) || is.na(file)) next()
    ft[]<-list(path=file)
  }
  id_for_files(path)
}

#' Update a file in the database
#'
#' This function is used to update a file in the files database
#' @param id ID of the file to update
#' @inheritParams add_files
#' @return ID
#' @export
update_files <- function(id=numeric(),path=character()) {
  ft<-files_table()
  ft[id]<-list(path=path)
  return(id)
}

#' Remove files from the database
#'
#' This function is used to remove files from the files database
#' @param ids IDs of the files
#' @export
remove_files <- function(ids) {
  ft<-files_table()
  ft[-ids]
}

#' Get the ID for a file
#'
#' @param paths path to one or more files
#' @return The ID(s) or NA if not found
#' @export
id_for_files <- function(paths) {
  if(!length(paths)) return(NA)
  paths=assert_file(paths)
  ft=files_table()
  tableIds=ft[,'id']
  tablePaths=ft[,'path']
  if(!length(tablePaths) || all(is.na(tablePaths))) tablePaths=''
  ids=c()
  for(file in paths) {
    exists=(tablePaths == file)
    if(!is.na(file) && any(exists)) {
      ids=append(ids,tableIds[exists][[1]])
    } else {
      ids=append(ids,NA)
    }
  }
  ids
}

#' Get the file for an ID
#'
#' @param ids id for one or more files
#' @return The file paths
#' @export
file_for_ids <- function(ids) {
  if(!length(ids)) return(c())
  ft=files_table()
  ft[ids,'path']
}
