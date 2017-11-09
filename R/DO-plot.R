#' Plot a data object
#'
#' This function plots a data object
#' @param x The object to plot
#' @examples
#' plot(data)
#' @export
plot.jms.data.object <- function(x,offset=1/sqrt(length(ycol(x))-1),xlim=NULL,ylim=NULL,y2lim=NULL,xlab=xlab_(x),ylab=ylab_(x),y2lab=y2lab_(x),axes=c(1,2),...) {
  if(length(ycol(x))>1) x=x+offset*seq(0,length(ycol(x))-1,1)*range(x)[[2]]

  if(any(is.null(xlim))) xlim=range(x[,xcol(x)][is.finite(x[,xcol(x)])])
  if(any(is.null(ylim))) ylim=extendrange(r=range(x),0.04)
  if(any(is.null(y2lim)) && !all(is.na(y2col(x)))) y2lim=range(x[,y2col(x)],na.rm = T)

  args=list(...)
  plot_args=args[names(args) %in% names(c(formals(axis),formals(pretty_plot)))]
  plot_args=plot_args[!names(plot_args) %in% c('col','lwd','lty')]
  plot_args=append(list(xlim=xlim,ylim=ylim,y2lim=y2lim,xlab=xlab,ylab=ylab,y2lab=y2lab,axes=axes),plot_args)
  do.call(pretty_plot,plot_args)

  lines_args=args[!names(args) %in% iplotArgBlacklist]
  lines_args=lines_args[!names(lines_args) %in% names(plot_args)]
  lines_args=append(list(x=x),lines_args)
  do.call(lines,lines_args)
}

#' Draw lines for a data object
#'
#' This function plots a data object
#' @param x The object to plot
#' @param y2 Include y2 data in plot?
#' @inheritParams graphics::plot.xy
#' @examples
#' plot.xy(data)
#' @export
lines.jms.data.object <- function(x,col=par('col'),type='l',y2=TRUE,...) {
  x_data=x[,xcol(x)]
  y_cols=ycol(x)
  y_df=if(all(is.na(y_cols))) c() else x[,y_cols]
  y2_cols=y2col(x)
  y2_df=if(all(is.na(y2_cols))) c() else x[,y2_cols]
  col_all=if(y2) expand_args(1:(length(y_cols)+length(y2_cols)),col)[[2]] else expand_args(1:length(y_cols),col)[[2]]
  type_all=if(y2) expand_args(1:(length(y_cols)+length(y2_cols)),type)[[2]] else expand_args(1:length(y_cols),type)[[2]]

  y2_=y2
  y2=NULL
  if(length(y_cols)<2) {
    x=data.frame(x=x_data,y=y_df)
    NextMethod(type=type_all[[1]])
  } else {
    for(i in 1:length(y_cols)) {
      x=data.frame(x=x_data,y=y_df[,i])
      col=col_all[[i]]
      NextMethod(type=type_all[[i]])
    }
  }
  if(y2_ && !all(is.na(y2_cols))) {
    if(length(y2_cols)<2) {
      x=data.frame(x=x_data,y=y2_df*plot_options$y2scale[[1]]+plot_options$y2scale[[2]])
      col=col_all[[1+length(y_cols)]]
      NextMethod(type=type_all[[1+length(y_cols)]])
    } else {
      for(i in 1:length(y2_cols)) {
        x=data.frame(x=x_data,y=y_df[,i]*plot_options$y2scale[[1]]+plot_options$y2scale[[2]])
        col=col_all[[i+length(y_cols)]]
        NextMethod(type=type_all[[i+length(y_cols)]])
      }
    }
  }
}
