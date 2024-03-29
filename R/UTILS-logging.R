#' Enable debug logging
#'
#' @details
#' Enables logging output. Logging operates under a hierarchal set of namespaces, each
#' namespace can have its own threshold. Currently the predefined namespaces are:
#' \describe{
#'  \item{[package-name]}{The default logger is the name of the package which is doing the logging.
#'                        Note . is replaced by - in the name if necessary. (inherits from jms-logging)}
#'  \item{jms-database}{All database logging falls under this namespace (inherits from jms-logging)}
#'  \item{jms-database-table}{All database table logging falls under this namespace (inherits from jms-database)}
#'  \item{jms-logging}{The root logger for all packages developed by me}
#' }
#'
#' @param threshold (optional for jms.enable.logging) see \code{\link[futile.logger]{flog.threshold}}
#' @param ns Logging namespace (see details)
#' @param reset If \code{TRUE}, reset the settings for any inherited loggers to match the default
#' @export
#' @rdname jms.logging
jms.enable.logging <- function(threshold) {
  if (!missing(threshold)) {
    jms.logging.threshold(threshold)
  }

  layout <- futile.logger::layout.format("~l ~t ~m")
  futile.logger::flog.layout(layout, name="jms-logging")
  log_message <- function(msg, ..., level, styleFun=NULL, ns=NULL, formattedMsg=FALSE) {
    the.namespace <- futile.logger::flog.namespace(-8)
    the.namespace <- ifelse(the.namespace == "futile.logger", "ROOT", the.namespace)
    the.function <- tryCatch(deparse(sys.call(-2)[[1]]),
                             error=function(e) "(shell)"
    )

    ns <- if(is.null(ns)) gsub('.', '-', the.namespace, fixed=T) else ns
    ns <- paste("jms-logging", ns, sep=".")

    logger <- futile.logger::flog.logger(ns)
    if (level > logger$threshold) return()

    if (formattedMsg) {
      prefix <- futile.logger::flog.layout(ns)(level, "[%s: %s]", the.namespace, the.function)
      if (!is.null(styleFun)) prefix <- styleFun(prefix)
      out <- paste0(prefix, sprintf(msg, ...))
      futile.logger::flog.appender(ns)(out)
    } else {
      out <- futile.logger::flog.layout(ns)(level, paste0("[%s: %s] ", msg), the.namespace, the.function, ...)
      if (!is.null(styleFun)) out <- styleFun(out)
      futile.logger::flog.appender(ns)(out)
    }
  }
  utils::assignInNamespace("log_message", log_message, ns="jms.classes")

  log.info("Logging successfully enabled")
}

#' @export
#' @rdname jms.logging
jms.logging.threshold <- function(threshold, ns=NULL, reset=FALSE) {
  name <- "jms-logging"
  if (!is.null(ns)) {
    name <- paste(name, ns, sep=".")
  }

  if (reset) {
    child_loggers <- substring(
      # Find loggers that inherit from jms-logging
      grep("^logger\\.jms-logging\\.",
        names(futile.logger::logger.options()),
        value=TRUE
      ),
      # Remove "logger." from the start of the string
      8
    )
    # This removes any custom logging settings for them
    futile.logger::flog.remove(child_loggers)
  }

  futile.logger::flog.threshold(get(threshold, envir=environment(futile.logger::flog.threshold)), name=name)
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

# as futile.logger::appender.tee, but strips styling information to file output
# e.g. shiny stack traces
jms.appender.tee <- function(file) {
  function(line) {
    cat(line, sep = "")
    cat(crayon::strip_style(line), file = file, append = TRUE, sep = "")
  }
}

#' @export
#' @rdname jms.logging
jms.logging.file <- function(file) {
  if (!is.element("futile.logger", utils::installed.packages()[, 1])) {
    stop("Please install futile.logger to continue")
  }
  app <- futile.logger::flog.appender("jms-logging")
  futile.logger::flog.appender(jms.appender.tee(file), name="jms-logging")
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
#' @usage log.trace(msg, ..., ns=NULL)
#' @rdname jms.logging
log.trace <- function(msg, ...) log_message(msg, ..., level=structure(9L, .Names="TRACE"), styleFun=style_trace)
#' @export log.debug
#' @usage log.debug(msg, ..., ns=NULL)
#' @rdname jms.logging
log.debug <- function(msg, ...) log_message(msg, ..., level=structure(8L, .Names="DEBUG"), styleFun=style_debug)
#' @export log.info
#' @usage log.info(msg, ..., ns=NULL)
#' @rdname jms.logging
log.info <- function(msg, ...) log_message(msg, ..., level=structure(6L, .Names="INFO"), styleFun=style_info)
#' @export log.warn
#' @usage log.warn(msg, ..., ns=NULL)
#' @rdname jms.logging
log.warn <- function(msg, ...) log_message(msg, ..., level=structure(4L, .Names="WARN"), styleFun=style_warn)
#' @export log.error
#' @usage log.error(msg, ..., ns=NULL)
#' @rdname jms.logging
log.error <- function(msg, ...) log_message(msg, ..., level=structure(2L, .Names="ERROR"), styleFun=style_error)
#' @export log.fatal
#' @usage log.fatal(msg, ..., ns=NULL)
#' @rdname jms.logging
log.fatal <- function(msg, ...) log_message(msg, ..., level=structure(1L, .Names="FATAL"), styleFun=style_fatal)
