#' Get a database table from a database
#'
#' @export
`[[.jms.database` <- function(x,name,...,reactiveSession=NULL,reactiveUpdateFreq=4000) {
  if(startsWith(name,'.')) return(get(name,envir=x))
  #Check for new tables
  x<-load(x)
  #Find table
  i=which(x$.table_names==name)
  if(!length(i)) stop('Database table "',name,'" was not found')
  log.info('Getting table %s',name)
  table=if(exists(name,envir=x)) get(name,envir=x) else NULL
  if(!is.null(x$.path)) {
    tablePath<-paste0(x$.path,'/',name,'.table')
    if(!file.exists(tablePath)) stop('Unable to load table ',name)
    dg=digest::digest(tablePath, algo = "sha1", file = T)
    if(x$.tableHashes[[i]]!=dg) {
      log.debug('Reloading table %s',name)
      ll<-loadTable(tablePath)
      if(is.null(table)) {
        table=ll$table
        parent.env(table)<-x
        table$.name<-name
        assign(name, table, envir = x)
      } else {
        #We shouldn't replace the table environement, just the data
        for(n in ls(ll$table, all.names=TRUE)) assign(n, get(n, ll$table), table)
      }
      x$.tableHashes[[i]]<-ll$hash
    } else {
      log.debug('Table does not need reloading')
    }
  } else {
    log.debug('Table is not stored on disk')
  }
  if(is.null(reactiveSession)) return(table)
  shiny::reactivePoll(reactiveUpdateFreq, reactiveSession,
                      checkFunc = function() {file.info(tablePath)$mtime},
                      valueFunc = function() {as.data.frame(x[[name]])})
}
#' @export
`[.jms.database`<-function(x,name,...) {
  `[[`(x,name,...)
}
#' @export
`$.jms.database`<-function(x,name,...) {
  `[[`(x,name,...)
}
