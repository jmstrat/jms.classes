#' Create a database table object including support for an ID column
#'
#' This function is used to initialise a database table
#' @param ... parameters are passed to \code{\link{data.frame}}
#' @param version Can be used to track changes to table schema
#' @return A database table object
#' @export
jms.database.table.id <- function(...,validator=NULL) {
  as.jms.database.table.id(data.frame(...,stringsAsFactors = FALSE), validator=validator)
}

#' Check if an object is a jms.database.table.id
#'
#' @param x The object to be tested
#' @return TRUE / FALSE
#' @export
is.jms.database.table.id <- function(x) {
  return(inherits(x,"jms.database.table.id"))
}

#' Convert an object into a jms.database.table.id
#'
#' @param x The object to be converted
#' @return The converted object
#' @export
as.jms.database.table.id <- function(x,validator,version) UseMethod("as.jms.database.table.id")


#' @method as.jms.database.table.id default
#' @export
as.jms.database.table.id.default <- function(x,validator,version) {
  stop("Unable to convert this class")
}

#' @method as.jms.database.table.id data.frame
#' @export
as.jms.database.table.id.data.frame <- function(x,validator=NULL,version=1) {
  if(!is.null(validator)) {
    validator_wrap <- function(id) {
      f=validator #Store local copy
      id=assert_positive(id,'id must be a positive numeric')
      cl<-match.call()
      m<-match('id',names(cl),0L)
      cl<-cl[-m]
      cl<-cl[-1L]
      ret<-do.call(f,as.list(cl))
      ret$id=id
      ret
    }
    formals(validator_wrap)<-append(formals(validator_wrap),formals(validator))
  } else {
    validator_wrap <- function(id) {
      assert_positive(id,'id must be a positive numeric')
      list(id=id)
    }
  }
  cn<-colnames(x)
  if(nrow(x)) {
    x[,'id']<-1:nrow(x)
  } else {
    x[,'id']<-numeric()
  }
  if(length(cn)) x<-x[,c('id',cn)]
  x<-as.jms.database.table(x,validator=validator_wrap)
  class(x)<-c('jms.database.table.id',class(x))
  x
}

#' @method as.jms.database.table.id jms.database.table
#' @export
as.jms.database.table.id.jms.database.table <- function(x,...) {
  as.jms.database.table.id.data.frame(as.data.frame(x),validator=x$.validator)
}

