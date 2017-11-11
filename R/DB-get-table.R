#' Get a database table from a database
#'
#' @export
`[[.jms.database` <- function(x,name,...,.internal=FALSE) {
  if(.internal || startsWith(name,'.')) return(get(name,envir=x))
  if(!length(which(x$.table_names==name))) stop('Database table "',name,'" was not found')
  log.info('Getting table %s',name)
  #Do any necessary reloading, then return the table
  x<-load(x)
  table<-load(get(name,envir=x))
  x[[name,.internal=TRUE]]<-table
  return(get(name,envir=x))
}
#' @export
`[.jms.database`<-function(x,name,...,.internal=FALSE) {
  `[[`(x,name,...,.internal)
}
#' @export
`$.jms.database`<-function(x,name,...,.internal=FALSE) {
  `[[`(x,name,...,.internal)
}
