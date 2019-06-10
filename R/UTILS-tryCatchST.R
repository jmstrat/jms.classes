#' TryCatch with a stack trace

#' @param expr expression to execute
#' @param error function of form error(e, stack_trace)
#' @export
tryCatchST = function(expr, error) {
  errorResult <- NULL
  errorTracer = function(obj) {
    # Storing the call stack
    calls = sys.calls()
    obj$call <- sys.call(-4)
    errorResult <<- error(obj, calls)
  }
  errorIgnorer <- function(obj) {}
  parentenv <- parent.frame()
  logErrors <- function(expr) {
    .Internal(.addCondHands('error', list(errorIgnorer), parentenv, environment(), FALSE))
    .Internal(.addCondHands('error', list(errorTracer), parentenv, NULL, TRUE))
    expr
  }
  value <- logErrors(return(expr))[[1L]]
  if(is.null(value)) errorResult else value
  # This is an attempt to simplify:
  # tryCatch(
  #   withCallingHandlers(expr,  error=errorTracer, caughtError=errorTracer),
  #   error = function(e) e
  # )
}

#' @export
#' @rdname tryCatchST
formatST <- function(st) {
  st <- rev(st)
  st <- sapply(strsplit(as.character(st), '\n'), function(x) x[[1]])
  paste0('#', 1:length(st), ' ', st, sep=' ', collapse='\n')
}
