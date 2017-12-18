config_dir=NULL
config_db<-NULL
persistent_settings<-NULL

.onLoad <- function(libname, pkgname) {
  config_dir<<-rappdirs::user_config_dir('jms.packages', 'jms')
  dir.create(config_dir, showWarnings = FALSE,recursive=TRUE)
  #Load config db
  db_dir<-file.path(config_dir,'config-database')
  dir.create(db_dir, showWarnings = FALSE,recursive=TRUE)
  config_db<<-jms.database(db_dir)
  persistent_settings <<- tryCatch({
    config_db[['persistent_settings']]
  }, error = function(e) {
    config_db[['persistent_settings']]<-jms.database.table(key=character(),value=character())
    config_db[['persistent_settings']]
  })
  #Load the project db
  project_database()
  return(invisible())
}

#' @export
get_persistent_setting <- function(key) {
  get_key(key)
}

#' @export
set_persistent_setting <- function(key,value) {
  set_key(key, value)
}

get_key <- function(key, table=persistent_settings) {
  i=which(key == table[,'key'])
  if(length(i)) {
    table[i,'value']
  } else {
    NULL
  }
}

set_key <- function(key, value, table=persistent_settings) {
  i=which(key == table[,'key'])
  if(length(i)) {
    table[i,'value']<-value
  } else {
    table[nrow(table)+1,]<-list(key,value)
  }
}
