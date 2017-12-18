#' Enable debug logging
#' @export
jms.enable.logging <- function() {
  if(is.element('futile.logger', installed.packages()[,1])) {
    #Ignore the nasty hacks to get the namespace and function to print correctly ;)
    f<-futile.logger::flog.namespace
    formals(f)$where=-10
    assignInNamespace('flog.namespace',f,ns='futile.logger')
    layout<-futile.logger::layout.format("~l ~t [~n: ~f] ~m")
    environment(layout)$where=-4
    futile.logger::flog.layout(layout, name='jms-logging')
    log_message <- function(msg,...,level) {
      ns<-get0('.jms.logger',envir=-3)
      if(is.null(ns)) {
        ns <- 'jms-logging'
      } else {
        ns <- paste('jms-logging',ns,sep='.')
      }
      futile.logger:::.log_level(msg, ..., level=level, name = ns,capture=FALSE)
    }
    assignInNamespace('log_message',log_message,ns='jms.classes')
  } else {
    log_message <- function(msg,...,level) {
      print(sprintf(msg, ...))
    }
    assignInNamespace('log_message',log_message,ns='jms.classes')
  }
}
#' @export
jms.logging.setnamespace <- function(name) {
  assign('.jms.logger',name,parent.frame())
}
#' @export
jms.logging.threshold <- function(threshold,ns=NULL) {
  if(is.null(ns)) {
    ns <- 'jms-logging'
  } else {
    ns <- paste('jms-logging',ns,sep='.')
  }
  futile.logger::flog.threshold(get(threshold,envir=environment(futile.logger::flog.threshold)), name = ns)
  return(invisible())
}
#' @export
jms.disable.logging <- function() {
  log_message <- function(msg,...,level) {return()}
  assignInNamespace('log_message',log_message,ns='jms.classes')
}

log_message <- function(msg,...,level) {return()}

#' @export log.trace
log.trace <- function(msg,...)  log_message(msg,...,level=structure(9L, .Names = "TRACE"))
#' @export log.debug
log.debug <- function(msg,...)  log_message(msg,...,level=structure(8L, .Names = "DEBUG"))
#' @export log.info
log.info <-  function(msg,...)  log_message(msg,...,level=structure(6L, .Names = "INFO"))
#' @export log.warn
log.warn <-  function(msg,...)  log_message(msg,...,level=structure(4L, .Names = "WARN"))
#' @export log.error
log.error <- function(msg,...)  log_message(msg,...,level=structure(2L, .Names = "ERROR"))
#' @export log.fatal
log.fatal <- function(msg,...)  log_message(msg,...,level=structure(1L, .Names = "FATAL"))
