iplotArgBlacklist<-c('labels','group') #the plot() command will ignore these
#' @inheritParams graphics::plot.window
#' @inheritParams graphics::plot.default
#' @inheritParams graphics::par
#' @inheritParams graphics::plot.xy
#' @param group Group to associate this plot with.
#' The x-axis zoom level of plots within a group is automatically synchronized.
#' @rdname iPlot
#' @export
iplot.jms.data.object <- function(...,offset=1/sqrt(length(ycol(data))-1),xlim=NULL,ylim=NULL,y2lim=NULL,axes=c(1,2),xlab=xlab_(data),ylab=ylab_(data),y2lab=y2lab_(data),col=par('col'),lwd=1,pch=NA,labels=NULL,group=NULL) {
  data<-combine.data.objects(unname(list(...)),interpolate=TRUE) #Need to interpolate to avoid gaps...
  dots <- substitute(list(...))[-1]
  argNames=c(sapply(dots, deparse))
  y1end=length(ycol(data))
  allCols=c(xcol(data),ycol(data),y2col(data))
  data<-data[,allCols[!is.na(allCols)]]
  if(ncol(data) > 2 && length(argNames)==ncol(data)-1) names(data)[2:length(data)]<-argNames

  if(length(ycol(data))>1) data=data+offset*seq(0,length(ycol(data))-1,1)*range(data)[[2]]
  if(any(is.null(xlim))) xlim=range(data[,xcol(data)][is.finite(data[,xcol(data)])])
  if(any(is.null(ylim))) ylim=extendrange(r=range(data),0.04)
  if(any(is.null(y2lim)) && !all(is.na(y2col(data)))) y2lim=range(data[,y2col(data)],na.rm = T)

  xrange=range(data[,1])
  if(xlim[[2]]>xrange[[2]]) xlim[[2]]<-xrange[[2]]
  if(xlim[[1]]<xrange[[1]]) xlim[[1]]<-xrange[[1]]

  graph<-dygraphs::dygraph(data,group=group)
  col_all=if(is.null(col)) NULL else expand_args(2:(ncol(data)),col)[[2]]
  lwd_all=if(is.null(lwd)) NULL else expand_args(2:(ncol(data)),lwd)[[2]]
  pch_all=if(is.null(pch)) NULL else expand_args(2:(ncol(data)),pch)[[2]]

  for(i in 1:(ncol(data)-1)) {
    col=col_all[[i]]
    drawPoints <- if(!is.na(pch_all[[i]])) TRUE else NULL
    pointSize <- if(!is.na(pch_all[[i]])) 1 else 0
    strokeWidth <- lwd_all[[i]]
    label<- if(!is.null(labels)) labels[[i]] else NULL
    axis<- if(i>y1end) 'y2' else 'y'
    graph<-dygraphs::dySeries(graph,label=label,color=rgb(t(col2rgb(col)/255)),axis=axis,drawPoints=drawPoints,pointSize=pointSize,strokeWidth=strokeWidth)
  }
  graph<-dyAxis.jms(graph,'x',label=expressionToHTML(xlab),valueRange=xlim,ticks=1%in%axes)
  graph<-dyAxis.jms(graph,'y',label=expressionToHTML(ylab),valueRange=ylim,ticks=2%in%axes)
  if(4%in%axes) graph<-dyAxis.jms(graph,'y2',label=expressionToHTML(y2lab),valueRange=y2lim,ticks=TRUE)
  graph <- dyBox(graph)
  direction <- if(any(c(2,4)%in%axes)) "both" else "vertical"
  graph <- dygraphs::dyCrosshair(graph,direction = direction)
  graph <- dygraphs::dyLegend(graph,show = "always", hideOnMouseOut = TRUE)

  graph$x$css <- '.dygraph-legend {background-color: transparent !important}'
  #Fix for mysterious warning...
  set.seed(1)
  #Return the graph (will plot at top level)
  ilayout.addPlot(graph)
}
