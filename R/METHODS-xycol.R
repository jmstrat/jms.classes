#' Get or set the x or y column(s) for an object
#'
#' @param x The object
#' @return The column(s)
#' @rdname xycols.jms
#' @export
xcol <- function(x) UseMethod("xcol")
#' @export
xcol.default <- function(x) {
  stop("Unable to get x data for this class")
}
#' @rdname xycols.jms
#' @export
`xcol<-` <- function(x, value) UseMethod("xcol<-")
#' @export
`xcol<-.default` <- function(x, value) {
  stop("Unable to assign x column for this class")
}
#' @rdname xycols.jms
#' @export
ycol <- function(x) UseMethod("ycol")
#' @export
ycol.default <- function(x) {
  stop("Unable to get y data for this class")
}
#' @rdname xycols.jms
#' @export
`ycol<-` <- function(x, value) UseMethod("ycol<-")
#' @export
`ycol<-.default` <- function(x, value) {
  stop("Unable to assign y column for this class")
}
#' @rdname xycols.jms
#' @export
y2col <- function(x) UseMethod("y2col")
#' @export
y2col.default <- function(x) {
  stop("Unable to get y2 data for this class")
}
#' @rdname xycols.jms
#' @export
`y2col<-` <- function(x, value) UseMethod("y2col<-")
#' @export
`y2col<-.default` <- function(x, value) {
  stop("Unable to assign y2 column for this class")
}
