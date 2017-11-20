#' Get a database table from a database
#'
#' @export
`[[.jms.database` <- function(x,name,...,.internal=FALSE) {
  if(.internal || startsWith(name,'.')) return(get(name,envir=x))
  #Check for new tables
  x<-load(x)
  #Find table
  i=which(x$.table_names==name)
  if(!length(i)) stop('Database table "',name,'" was not found')
  log.info('Getting table %s',name)
  tablePath<-paste0(x$.path,'/',name,'.table')
  if(!file.exists(tablePath)) stop('Unable to load table ',name)
  mt=file.info(tablePath)$mtime
  if(is.na(mt)) mt=.POSIXct(0)
  tableEnv=get(name,envir=x)
  if(x$.tableModTimes[[i]]<mt) {
    log.info('Reloading table %s',name)
    make_lockfile(paste0(tablePath,'.lock'))
    table<-readRDS(tablePath)
    remove_lockfile(paste0(tablePath,'.lock'))
    assign('.table',table,envir=tableEnv)
    x$.tableModTimes[[i]]<-file.info(tablePath)$mtime
    x[[name,.internal=TRUE]]<-tableEnv
  }
  return(tableEnv)
}
#' @export
`[.jms.database`<-function(x,name,...,.internal=FALSE) {
  `[[`(x,name,...,.internal)
}
#' @export
`$.jms.database`<-function(x,name,...,.internal=FALSE) {
  `[[`(x,name,...,.internal)
}
