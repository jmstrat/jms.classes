#' Create a database  object
#'
#' This function is used to initialise a reactive database
#' @details A database object holds \code{\link{jms.database.table}} objects
#' @param path Path to the directory containing the database on disk
#' @param reactiveSession The \code{shiny} session object
#' @param reactiveUpdateFreq Approximate number of milliseconds to wait between polling for updates.
#' @return Database object
#' @export
jms.reactive.database <- function(path=NULL,reactiveSession=NULL,reactiveUpdateFreq=4000) {
  as.jms.reactive.database(jms.database(path=path),reactiveSession,reactiveUpdateFreq)
}

#' Check if an object is a jms.reactive.database
#'
#' @param x The object to be tested
#' @return TRUE / FALSE
#' @export
is.jms.reactive.database <- function(x) {
  return(inherits(x,"jms.reactive.database"))
}

#' Convert an object into a jms.database
#'
#' @param x The object to be converted
#' @return The converted object
#' @export
as.jms.reactive.database <- function(x,reactiveSession=NULL,reactiveUpdateFreq=4000) UseMethod("as.jms.reactive.database")

#' @export
as.jms.reactive.database.default <- function(x,reactiveSession=NULL,reactiveUpdateFreq=4000) {
  tryCatch(as.jms.reactive.database(as.jms.database(x),reactiveSession=reactiveSession,reactiveUpdateFreq=reactiveUpdateFreq),error = function(e) stop("Unable to convert this class"))
}

#' @method as.jms.reactive.database jms.database
#' @export
as.jms.reactive.database.jms.database <- function(x,reactiveSession=NULL,reactiveUpdateFreq=4000) {
  if(!inherits(reactiveSession,'ShinySession')) stop('reactiveSession must be a ShinySession object')
  self<-get0('.self',envir=x, ifnotfound=x)
  database.method <- database.method.generator(self,overwrite=TRUE)
  database.var <- database.var.generator(self,overwrite=TRUE)

  #Every time the hash updates, the reactive should invalidate
  rv<-shiny::reactiveValues()
  th<-self$.tableHashes
  for(n in names(th)) rv[[n]]<-th[[n]]
  database.var('.rv',rv)
  database.var('.reactiveSession',reactiveSession)

  force(reactiveUpdateFreq)
  if (is.function(reactiveUpdateFreq)) {
    database.var('.reactiveUpdateFreq',reactiveUpdateFreq)
  } else {
    database.var('.reactiveUpdateFreq',function() reactiveUpdateFreq)
  }

  updater <- function(name) {
    rv <- self$.rv
    if(is.null(self$.path)) {
      current=shiny::isolate(rv[[name]])
      if(!is.numeric(current) || !length(current)) current=0
      rv[[name]]<-current+1
    } else {
      rv[[name]]<-self$.tableHashes[[name]]
    }
  }

  st<-self$.saveTable
  saveTable <- function(name, database=x) {
    ret<-st(name=name,database=database)
    updater(name)
    ret
  }
  database.method('.saveTable',saveTable)

  attr(x, "class") <- c("jms.reactive.database",class(x))
  return(x)
}

#' @export
`[[.jms.reactive.database` <- function(x,name) {
  if(startsWith(name,'.')) return(get(name,envir=x))
  x$.loadDatabase()
  if(!name %in% x$.self$.table_names) stop('Database table "',name,'" was not found')
  rs <- x$.reactiveSession
  ruf <- x$.reactiveUpdateFreq
  rv <- x$.rv
  #Start polling to check for changes
  shiny::observe({
    if(!is.null(x$.path)) rv[[name]]<-x$.getHashForTable(name)
    shiny::invalidateLater(ruf(), rs)
  })
  shiny::reactive({
    rv[[name]] #Take reactive dependency (i.e. re-evaluate whenever the hash changes)
    as.data.frame(`[[.jms.database`(x,name)) #NextMethod
  }, label = NULL)
}
