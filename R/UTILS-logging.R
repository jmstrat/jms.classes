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
    futile.logger::flog.layout(layout, name='jms.logging')
    futile.logger::flog.threshold(futile.logger::DEBUG, name = 'jms.logging')
    log_message <- function(msg,...,level) {
      futile.logger:::.log_level(msg, ..., level=level, name = 'jms.logging',capture=FALSE)
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
jms.disable.logging <- function() {
  log_message <- function(msg,...,level) {return()}
  assignInNamespace('log_message',log_message,ns='jms.classes')
}

layout.format <- function (format, datetime.fmt = "%Y-%m-%d %H:%M:%S")
{
  where <- -1
  function(level, msg, ...) {
    if (!is.null(substitute(...)))
      msg <- sprintf(msg, ...)
    the.level <- names(level)
    the.time <- format(Sys.time(), datetime.fmt)
    the.namespace <- ifelse(flog.namespace() == "futile.logger",
                            "ROOT", flog.namespace())
    the.function <- tryCatch(deparse(sys.call(where)[[1]]),
                             error = function(e) "(shell)")
    the.function <- ifelse(length(grep("flog\\.", the.function)) ==
                             0, the.function, "(shell)")
    message <- gsub("~l", the.level, format, fixed = TRUE)
    message <- gsub("~t", the.time, message, fixed = TRUE)
    message <- gsub("~n", the.namespace, message, fixed = TRUE)
    message <- gsub("~f", the.function, message, fixed = TRUE)
    message <- gsub("~m", msg, message, fixed = TRUE)
    sprintf("%s\n", message)
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
