#' Enable debug logging
#' @export
jms.enable.logging <- function() {
  if(is.element('futile.logger', utils::installed.packages()[,1])) {
    layout<-futile.logger::layout.format("~l ~t ~m")
    futile.logger::flog.layout(layout, name='jms-logging')
    log_message <- function(msg,...,level, styleFun=NULL) {
      ns<-get0('.jms.logger',envir=-3)
      if(is.null(ns)) {
        ns <- 'jms-logging'
      } else {
        ns <- paste('jms-logging',ns,sep='.')
      }
      the.namespace <- futile.logger::flog.namespace(-8)
      the.namespace <- ifelse(the.namespace == "futile.logger",
                              "ROOT", the.namespace)
      the.function <- tryCatch(deparse(sys.call(-2)[[1]]),
                               error = function(e) "(shell)")
      force(the.namespace)
      force(the.function)
      msg = sprintf('[%s: %s] %s', the.namespace, the.function, msg)
      if(!is.null(styleFun)) {
        out = capture.output(futile.logger:::.log_level(msg, ..., level=level, name = ns,capture=FALSE))
        force(out)
        cat(styleFun(out), '\n')
      } else {
        futile.logger:::.log_level(msg, ..., level=level, name = ns,capture=FALSE)
      }
    }
    utils::assignInNamespace('log_message',log_message,ns='jms.classes')
  } else {
    log_message <- function(msg,...,level, styleFun=NULL) {
      print(sprintf(msg, ...))
    }
    utils::assignInNamespace('log_message',log_message,ns='jms.classes')
  }
  log.info('Logging successfully enabled')
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
  log.info('Disabling logging')
  log_message <- function(msg,...,level, styleFun) {return()}
  utils::assignInNamespace('log_message',log_message,ns='jms.classes')
}

log_message <- function(msg,...,level, styleFun) {return()}

style_trace <- NULL
style_debug <- NULL
style_info <- NULL
style_warn <- NULL
style_error <- NULL
style_fatal <- NULL
if(requireNamespace("crayon", quietly=TRUE)) {
  style_trace <- crayon::combine_styles("cyan", "blurred")
  style_debug <- crayon::combine_styles("cyan", "blurred")
  style_info <- crayon::yellow
  style_warn <- crayon::red
  style_error <- crayon::combine_styles("white", "bgRed")
  style_fatal <- crayon::combine_styles("bold", "white", "bgRed")
}

#' @export log.trace
log.trace <- function(msg,...)  log_message(msg,...,level=structure(9L, .Names = "TRACE"), styleFun=style_trace)
#' @export log.debug
log.debug <- function(msg,...)  log_message(msg,...,level=structure(8L, .Names = "DEBUG"), styleFun=style_debug)
#' @export log.info
log.info <-  function(msg,...)  log_message(msg,...,level=structure(6L, .Names = "INFO"), styleFun=style_info)
#' @export log.warn
log.warn <-  function(msg,...)  log_message(msg,...,level=structure(4L, .Names = "WARN"), styleFun=style_warn)
#' @export log.error
log.error <- function(msg,...)  log_message(msg,...,level=structure(2L, .Names = "ERROR"), styleFun=style_error)
#' @export log.fatal
log.fatal <- function(msg,...)  log_message(msg,...,level=structure(1L, .Names = "FATAL"), styleFun=style_fatal)
