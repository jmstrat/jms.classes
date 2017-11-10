#' @export
plot.jms.database.table <- function(x,...) {
  name=attr(x,'name')
  name=paste(toupper(substr(name, 1, 1)), substr(name, 2, nchar(name)), ' Database', sep="")
  entries=paste0(nrow(x),' Entries')
  params=paste0(nrow(x)*ncol(x),' Parameters')
  op <- par(mar = rep(0, 4),oma=rep(0, 4))
  plot.new()
  plot.window(c(0,1), c(0,1), xaxs = "i", yaxs = "i")
  rect(0,0.66,1,1,col=rgb(0.1176,0.6784,0.9922,0.5),border=NA)
  text(0.5,0.83,name)
  rect(0,0.33,1,0.66,col=rgb(0.1412,0.8039,0.9961,0.5),border=NA)
  text(0.5,0.49,entries)
  rect(0,0,1,0.33,col=rgb(0.1490,0.8627,0.9961,0.5),border=NA)
  text(0.5,0.17,params)

  par(op)
}
