#' @export
plot.jms.database <- function(x,...) {
  path=x$.path
  entries=paste0(length(x$.table_names),' Tables')
  op <- par(mar = rep(0, 4),oma=rep(0, 4))
  plot.new()
  plot.window(c(0,1), c(0,1), xaxs = "i", yaxs = "i")
  rect(0,0.66,1,1,col=rgb(240,117,15,maxColorValue = 255),border=NA)
  text(0.5,0.83,'Database')
  rect(0,0.33,1,0.66,col=rgb(244,128,32,maxColorValue = 255),border=NA)
  text(0.5,0.49,path)
  rect(0,0,1,0.33,col=rgb(240,149,55,maxColorValue = 255),border=NA)
  text(0.5,0.17,entries)

  par(op)
}
