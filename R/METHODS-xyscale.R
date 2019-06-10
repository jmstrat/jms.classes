#' Get or set the x or y column scale factors for a data object
#'
#' @param x The data object
#' @return The column scales
#' @rdname xyscales.jms
#' @export
xscale <- function(x) UseMethod("xscale")
#' @export
xscale.default <- function(x) {
  stop("Unable to get x data for this class")
}
#' @rdname xyscales.jms
#' @export
`xscale<-` <- function(x,value) UseMethod("xscale<-")
#' @export
`xscale<-.default` <- function(x,value) {
  stop("Unable to assign x column for this class")
}
#' @rdname xyscales.jms
#' @export
yscale <- function(x) UseMethod("yscale")
#' @export
yscale.default <- function(x) {
  stop("Unable to get y data for this class")
}
#' @rdname xyscales.jms
#' @export
`yscale<-` <- function(x,value) UseMethod("yscale<-")
#' @export
`yscale<-.default` <- function(x,value) {
  stop("Unable to assign y column for this class")
}
#' @rdname xyscales.jms
#' @export
y2scale <- function(x) UseMethod("y2scale")
#' @export
y2scale.default <- function(x) {
  stop("Unable to get y2 data for this class")
}
#' @rdname xyscales.jms
#' @export
`y2scale<-` <- function(x,value) UseMethod("y2scale<-")
#' @export
`y2scale<-.default` <- function(x,value) {
  stop("Unable to assign y2 column for this class")
}

#Internal methods for use in function definitions as default arguments
xscale_<-function(...)xscale(...)
yscale_<-function(...)yscale(...)
y2scale_<-function(...)y2scale(...)
