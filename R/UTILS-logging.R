#' Enable debug logging
#'
#' @param threshold (optional for jms.enable.logging) see \code{\link[futile.logger]{flog.threshold}}
#' @export
#' @rdname jms.logging
jms.enable.logging <- function(threshold) {
  if (!missing(threshold)) {
    jms.logging.threshold(threshold)
  }
  if (is.element("futile.logger", utils::installed.packages()[, 1])) {
    layout <- futile.logger::layout.format("~l ~t ~m")
    futile.logger::flog.layout(layout, name="jms-logging")
    log_message <- function(msg, ..., level, styleFun=NULL, ns=NULL) {
      the.namespace <- futile.logger::flog.namespace(-8)
      the.namespace <- ifelse(the.namespace == "futile.logger", "ROOT", the.namespace)
      the.function <- tryCatch(deparse(sys.call(-2)[[1]]),
        error=function(e) "(shell)"
      )

      ns <- if(is.null(ns)) the.namespace else ns
      ns <- paste("jms-logging", ns, sep=".")

      out <- capture.output(
        futile.logger:::.log_level(paste0("[%s: %s] ", msg),
          the.namespace, the.function, ...,
          level=level, name=ns, capture=FALSE
        )
      )

      if (length(out) == 0) {
        return()
      }
      if (!is.null(styleFun)) {
        cat(styleFun(out), sep="\n")
      } else {
        cat(out, sep="\n")
      }
    }
    utils::assignInNamespace("log_message", log_message, ns="jms.classes")
  } else {
    log_message <- function(msg, ..., level, styleFun=NULL) {
      cat(sprintf(msg, ...), sep="\n")
    }
    utils::assignInNamespace("log_message", log_message, ns="jms.classes")
    log_message("Install the futile.logger and crayon packages for improved logging.")
  }
  log.info("Logging successfully enabled")
}

#' @export
#' @rdname jms.logging
jms.logging.threshold <- function(threshold, ns=NULL) {
  if (is.null(ns)) {
    ns <- "jms-logging"
  } else {
    ns <- paste("jms-logging", ns, sep=".")
  }
  futile.logger::flog.threshold(get(threshold, envir=environment(futile.logger::flog.threshold)), name=ns)
  return(invisible())
}
#' @export
#' @rdname jms.logging
jms.disable.logging <- function() {
  log.info("Disabling logging")
  log_message <- function(msg, ..., level, styleFun) {
    return()
  }
  utils::assignInNamespace("log_message", log_message, ns="jms.classes")
}

#' @export
#' @rdname jms.logging
jms.logging.file <- function(file) {
  if (!is.element("futile.logger", utils::installed.packages()[, 1])) {
    stop("Please install futile.logger to continue")
  }
  app <- futile.logger::flog.appender("jms-logging")
  futile.logger::flog.appender(futile.logger::appender.tee(file), name="jms-logging")
  invisible(app)
}

#' @export
#' @rdname jms.logging
jms.logging.function <- function(app) {
  oapp <- futile.logger::flog.appender("jms-logging")
  futile.logger::flog.appender(app, name="jms-logging")
  invisible(oapp)
}

log_message <- function(msg, ..., level, styleFun) {
  return()
}

style_trace <- NULL
style_debug <- NULL
style_info <- NULL
style_warn <- NULL
style_error <- NULL
style_fatal <- NULL
if (requireNamespace("crayon", quietly=TRUE)) {
  style_trace <- crayon::combine_styles("cyan", "blurred")
  style_debug <- crayon::combine_styles("cyan", "blurred")
  style_info <- crayon::yellow
  style_warn <- crayon::red
  style_error <- crayon::combine_styles("white", "bgRed")
  style_fatal <- crayon::combine_styles("bold", "white", "bgRed")
}

#' @export log.trace
#' @name log.trace
#' @rdname jms.logging
log.trace <- function(msg, ...) log_message(msg, ..., level=structure(9L, .Names="TRACE"), styleFun=style_trace)
#' @export log.debug
#' @name log.debug
#' @rdname jms.logging
log.debug <- function(msg, ...) log_message(msg, ..., level=structure(8L, .Names="DEBUG"), styleFun=style_debug)
#' @export log.info
#' @name log.info
#' @rdname jms.logging
log.info <- function(msg, ...) log_message(msg, ..., level=structure(6L, .Names="INFO"), styleFun=style_info)
#' @export log.warn
#' @name log.warn
#' @rdname jms.logging
log.warn <- function(msg, ...) log_message(msg, ..., level=structure(4L, .Names="WARN"), styleFun=style_warn)
#' @export log.error
#' @name log.error
#' @rdname jms.logging
log.error <- function(msg, ...) log_message(msg, ..., level=structure(2L, .Names="ERROR"), styleFun=style_error)
#' @export log.fatal
#' @name log.fatal
#' @rdname jms.logging
log.fatal <- function(msg, ...) log_message(msg, ..., level=structure(1L, .Names="FATAL"), styleFun=style_fatal)
