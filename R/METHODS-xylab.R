#' Get or set the x or y column labels for a data object
#'
#' @param x The data object
#' @return The column labels
#' @rdname xylabels.jms
#' @export
xlab <- function(x) UseMethod("xlab")
#' @export
xlab.default <- function(x) {
  stop("Unable to get x data for this class")
}
#' @rdname xylabels.jms
#' @export
`xlab<-` <- function(x, value) UseMethod("xlab<-")
#' @export
`xlab<-.default` <- function(x, value) {
  stop("Unable to assign x column for this class")
}
#' @rdname xylabels.jms
#' @export
ylab <- function(x) UseMethod("ylab")
#' @export
ylab.default <- function(x) {
  stop("Unable to get y data for this class")
}
#' @rdname xylabels.jms
#' @export
`ylab<-` <- function(x, value) UseMethod("ylab<-")
#' @export
`ylab<-.default` <- function(x, value) {
  stop("Unable to assign y column for this class")
}
#' @rdname xylabels.jms
#' @export
y2lab <- function(x) UseMethod("y2lab")
#' @export
y2lab.default <- function(x) {
  stop("Unable to get y2 data for this class")
}
#' @rdname xylabels.jms
#' @export
`y2lab<-` <- function(x, value) UseMethod("y2lab<-")
#' @export
`y2lab<-.default` <- function(x, value) {
  stop("Unable to assign y2 column for this class")
}

# Internal methods for use in function definitions as default arguments
xlab_ <- function(...) xlab(...)
ylab_ <- function(...) ylab(...)
y2lab_ <- function(...) y2lab(...)
