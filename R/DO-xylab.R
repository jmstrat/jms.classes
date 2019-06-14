#' @export
xlab.jms.data.object <- function(x) {
  xlab <- attr(x, "x_type")
  if (is.null(xlab)) xlab <- "Unknown"
  xlab
}
#' @export
`xlab<-.jms.data.object` <- function(x, value) {
  attr(x, "x_type") <- value
  x
}
#' @export
ylab.jms.data.object <- function(x) {
  ylab <- attr(x, "y_type")
  if (is.null(ylab)) ylab <- "Unknown"
  ylab
}
#' @export
`ylab<-.jms.data.object` <- function(x, value) {
  attr(x, "y_type") <- value
  x
}
#' @export
y2lab.jms.data.object <- function(x) {
  y2lab <- attr(x, "y2_type")
  if (is.null(y2lab)) y2lab <- "Unknown"
  y2lab
}
#' @export
`y2lab<-.jms.data.object` <- function(x, value) {
  attr(x, "y2_type") <- value
  x
}
