#' Calculate Range
#'
#' This function calculates the range of a data objects
#' @param x The data object
#' @return
#' The range
#' @examples
#' range(data)
#' @export
range.jms.data.object <- function(x,offset=0,...) {
  if(!is.jms.data.object(x)) stop("x must be a jms.data.object")
  yc=ycol(x)
  if(offset>0) {
    ym=max(x[,ycol(x)])
    for(i in 1:length(yc)) x[,yc[[i]]]=x[,yc[[i]]]+offset*(i-1)*ym
  }
  range.default(x[,ycol(x)],na.rm=TRUE)
}
