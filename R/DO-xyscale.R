#' @export
xscale.jms.data.object <- function(x) {
  xscale=attr(x,'x_scale')
  if(is.null(xscale)) xscale=1
  xscale
}
#' @export
`xscale<-.jms.data.object` <- function(x,value) {
  attr(x,'x_scale')<-value
  x
}
#' @export
yscale.jms.data.object <- function(x) {
  yscale=attr(x,'y_scale')
  if(is.null(yscale)) yscale=1
  yscale
}
#' @export
`yscale<-.jms.data.object` <- function(x,value) {
  attr(x,'y_scale')<-value
  x
}
#' @export
y2scale.jms.data.object <- function(x) {
  y2scale=attr(x,'y2_scale')
  if(is.null(y2scale)) y2scale=1
  y2scale
}
#' @export
`y2scale<-.jms.data.object` <- function(x,value) {
  attr(x,'y2_scale')<-value
  x
}

get_scaled <- function(x) {
  if(!all(is.na(xcol(x)))) x[,xcol(x)] = x[,xcol(x)] * xscale(x)
  if(!all(is.na(ycol(x)))) x[,ycol(x)] = x[,ycol(x)] * yscale(x)
  if(!all(is.na(y2col(x)))) x[,y2col(x)] = x[,y2col(x)] * y2scale(x)
  x
}
