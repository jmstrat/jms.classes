#' Enable debug logging
#' @export
jms.enable.logging <- function() {
  if(is.element('futile.logger', installed.packages()[,1])) {
    futile.logger::flog.layout(futile.logger::layout.format("~l ~t [~f] ~m"), name='jms.logging')
    futile.logger::flog.threshold(futile.logger::DEBUG, name = 'jms.logging')
    log_message <- function(msg,...,level) {
      futile.logger:::.log_level(msg, ..., level=level, name = 'jms.logging',capture=FALSE)
    }
    assignInNamespace('log_message',log_message,ns='jms.classes')
  }
}

log_message <- function(msg,...,level) {return()}

#' @export
log.trace <- function(msg,...)  log_message(msg,...,level=structure(9L, .Names = "TRACE"))
#' @export
log.debug <- function(msg,...)  log_message(msg,...,level=structure(8L, .Names = "DEBUG"))
#' @export
log.info <-  function(msg,...)  log_message(msg,...,level=structure(6L, .Names = "INFO"))
#' @export
log.warn <-  function(msg,...)  log_message(msg,...,level=structure(4L, .Names = "WARN"))
#' @export
log.error <- function(msg,...)  log_message(msg,...,level=structure(2L, .Names = "ERROR"))
#' @export
log.fatal <- function(msg,...)  log_message(msg,...,level=structure(1L, .Names = "FATAL"))
