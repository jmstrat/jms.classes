#' Get a database table from a database
#'
#' @export
`[[.jms.database` <- function(x,name,...,.internal=FALSE,reactiveSession=NULL,reactiveUpdateFreq=4000) {
  if(.internal || startsWith(name,'.')) return(get(name,envir=x))
  #Check for new tables
  x<-load(x)
  #Find table
  i=which(x$.table_names==name)
  if(!length(i)) stop('Database table "',name,'" was not found')
  log.info('Getting table %s',name)
  tableEnv=get(name,envir=x)
  if(!is.null(x$.path)) {
    tablePath<-paste0(x$.path,'/',name,'.table')
    if(!file.exists(tablePath)) stop('Unable to load table ',name)
    dg=digest::digest(tablePath, algo = "sha1", file = T)
    if(x$.tableHashes[[i]]!=dg) {
      log.info('Reloading table %s',name)
      make_lockfile(paste0(tablePath,'.lock'))
      table<-readRDS(tablePath)
      remove_lockfile(paste0(tablePath,'.lock'))
      assign('.table',table,envir=tableEnv)
      x$.tableHashes[[i]]<-digest::digest(tablePath, algo = "sha1", file = T)
      x[[name,.internal=TRUE]]<-tableEnv
    }
  }
  if(is.null(reactiveSession)) return(tableEnv)
  shiny::reactivePoll(reactiveUpdateFreq, reactiveSession,
                      checkFunc = function() {file.info(tablePath)$mtime},
                      valueFunc = function() {as.data.frame(x[[name]])})
}
#' @export
`[.jms.database`<-function(x,name,...,.internal=FALSE) {
  `[[`(x,name,...,.internal)
}
#' @export
`$.jms.database`<-function(x,name,...,.internal=FALSE) {
  `[[`(x,name,...,.internal)
}
