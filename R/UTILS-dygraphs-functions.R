#' Add a box to a \code{\link[dygraphs]{dygraph}}
#'
#' @export
dyBox <- function(dygraph,lwd=1) {
  dygraphs::dyCallbacks(dygraph,underlayCallback=htmlwidgets::JS(paste0("function(ctx, area, dygraph) {
                                                                        ctx.lineWidth=",lwd*2,";
                                                                        ctx.strokeStyle = 'black';
                                                                        ctx.strokeRect(area.x, area.y, area.w, area.h);
}")))
}

#' Set the x limits for a \code{\link[dygraphs]{dygraph}}
#'
#' @export
dyxlim <- function(dygraph,xlim) {
  dygraph$x$attrs$dateWindow<-xlim
  dygraph
}

#' Default axis for a \code{\link[dygraphs]{dygraph}}
#' @inheritParams dygraphs::dyAxis
#' @param ticks Show ticks?
#' @param digits Number of decimal places in legend (only relevant if ticks==TRUE)
#'
#' @export
dyAxis.jms <- function(graph,name,label,valueRange,ticks=TRUE,digits=2) {
  graph<- if(ticks)
    dygraphs::dyAxis(graph,name,label=label,valueRange=valueRange, drawGrid = FALSE,axisLineWidth = 1,independentTicks=TRUE,
                   valueFormatter=paste0('function(d){return parseFloat(Math.round(d*100)/100).toFixed(',digits,')}'))
  else
    dygraphs::dyAxis(graph,name,label=label,valueRange=valueRange, drawGrid = FALSE,axisLineWidth = 1,
                     axisLabelWidth=20,ticker='function(x) {return[]}',
                     valueFormatter=' function(x) {return("")}')

  if(name=='x') graph<-dyxlim(graph,valueRange)
  graph
}
