#' @export
xcol.jms.data.object <- function(x) {
  xcol=attr(x,'x_column')
  if(is.null(xcol)) {
    if(ncol(x)<=1) stop('Cannot get x column')
    warning('Data type unknown, assuming 1st column for x axis')
    xcol=1
  }
  xcol
}
#' @export
`xcol<-.jms.data.object` <- function(x,value) {
  if(!is.numeric(value)) {
    value = match(value ,colnames(x))
  }
  attr(x,'x_column')<-value
  x
}
#' @export
ycol.jms.data.object <- function(x) {
  ycol=attr(x,'y_column')
  if(is.null(ycol)) {
    if(ncol(x)<=1) stop('Cannot get y column')
    warning('Data type unknown, assuming 2nd column for y axis')
    ycol=2
  }
  ycol
}
#' @export
`ycol<-.jms.data.object` <- function(x,value) {
  if(!is.numeric(value)) {
    value = match(value ,colnames(x))
  }
  attr(x,'y_column')<-value
  x
}
#' @export
y2col.jms.data.object <- function(x) {
  ycol=attr(x,'y2_column')
  if(is.null(ycol)) {
    if(ncol(x)<=1) stop('Cannot get y2 column')
    warning('Data type unknown, assuming no y2 axis')
    ycol = NA_integer_
  }
  ycol
}
#' @export
`y2col<-.jms.data.object` <- function(x,value) {
  if(!is.numeric(value)) {
    value = match(value, colnames(x))
  }
  attr(x,'y2_column')<-value
  x
}
