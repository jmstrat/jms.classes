iplotArgBlacklist <- c("labels", "group") # the plot() command will ignore these
#' @inheritParams graphics::plot.window
#' @inheritParams graphics::plot.default
#' @inheritParams graphics::par
#' @inheritParams graphics::plot.xy
#' @param group Group to associate this plot with.
#' The x-axis zoom level of plots within a group is automatically synchronized.
#' @rdname iPlot
#' @export
iplot.jms.data.object <- function(..., offset=1 / sqrt(length(ycol(data)) - 1),
                                  xlim=NULL, ylim=NULL, y2lim=NULL, axes=c(1, 2),
                                  xlab=xlab_(data), ylab=ylab_(data), y2lab=y2lab_(data),
                                  col=graphics::par("col"), lwd=1, pch=NA, labels=NULL,
                                  group=NULL, unsortedRangeWorkaround=T, type="l") {
  data <- combine(unname(list(...)), interpolate=TRUE) # Need to interpolate to avoid gaps...
  dots <- substitute(list(...))[-1]
  if (length(labels)) {
    argNames <- labels
  } else {
    argNames <- c(sapply(dots, deparse))
    if (utils::object.size(argNames) > 1000) argNames <- NULL
  }
  y1end <- length(ycol(data))
  allCols <- c(xcol(data), ycol(data), y2col(data))

  # To allow unit conversions etc.
  data <- get_scaled(data)

  data <- data[, allCols[!is.na(allCols)]]
  if (ncol(data) > 2 && length(argNames) == ncol(data) - 1) names(data)[2:length(data)] <- argNames

  xrange <- range(data[, xcol(data)][is.finite(data[, xcol(data)])], na.rm=T)

  if (length(ycol(data)) > 1) data <- data + offset * seq(0, length(ycol(data)) - 1, 1) * range(data)[[2]]
  if (any(is.null(xlim))) xlim <- xrange
  if (any(is.null(ylim))) ylim <- grDevices::extendrange(r=range(data), 0.04)
  if (any(is.null(y2lim)) && !all(is.na(y2col(data)))) y2lim <- range(data[, y2col(data)], na.rm=T)

  i <- 1
  j <- 2
  if (xlim[[1]] > xlim[[2]]) {
    j <- 1
    i <- 2
  }
  if (xlim[[j]] > xrange[[2]]) xlim[[j]] <- xrange[[2]]
  if (xlim[[i]] < xrange[[1]]) xlim[[i]] <- xrange[[1]]

  if (unsortedRangeWorkaround) {
    # By default dygraphs expects data to be sorted in x
    # Thus, when it "resets" the xrange it simply set it to cover
    # the range between the first and last values, this is not
    # necessarily true of unsorted data, so we add two invisible
    # points at the start and the end to trick it.
    data <- rbind(NA, data, NA)
    data[1, 1] <- xlim[[1]]
    data[nrow(data), 1] <- xlim[[2]]
  }

  graph <- dygraphs::dygraph(data, group=group)
  col_all <- if (is.null(col)) NULL else expand_args(2:(ncol(data)), col)[[2]]
  lwd_all <- if (is.null(lwd)) NULL else expand_args(2:(ncol(data)), lwd)[[2]]
  pch_all <- if (is.null(pch)) NULL else expand_args(2:(ncol(data)), pch)[[2]]

  for (i in 1:(ncol(data) - 1)) {
    col <- col_all[[i]]
    drawPoints <- if (!is.na(pch_all[[i]]) || type == "p") TRUE else NULL
    pointSize <- if (!is.na(pch_all[[i]]) || type == "p") 2 else 0
    strokeWidth <- lwd_all[[i]]
    label <- if (!is.null(labels) && length(labels) >= i) labels[[i]] else NULL
    axis <- if (i > y1end) "y2" else "y"
    graph <- dygraphs::dySeries(
      graph,
      label=label, color=grDevices::rgb(t(grDevices::col2rgb(col) / 255)),
      axis=axis, drawPoints=drawPoints, pointSize=pointSize, strokeWidth=strokeWidth
    )
  }
  graph <- Plotting.Utils::dyAxis.jms(graph, "x", label=expressionToHTML(xlab), valueRange=xlim, ticks=1 %in% axes)
  graph <- Plotting.Utils::dyAxis.jms(graph, "y", label=expressionToHTML(ylab), valueRange=ylim, ticks=2 %in% axes)
  if (4 %in% axes) {
    graph <- Plotting.Utils::dyAxis.jms(graph, "y2", label=expressionToHTML(y2lab), valueRange=y2lim, ticks=TRUE)
  }
  graph <- Plotting.Utils::dyBox(graph)
  direction <- if (any(c(2, 4) %in% axes)) "both" else "vertical"
  graph <- dygraphs::dyCrosshair(graph, direction=direction)
  graph <- dygraphs::dyLegend(graph, show="always", hideOnMouseOut=TRUE)

  graph$x$css <- ".dygraph-legend {background-color: transparent !important}"
  # Fix for mysterious warning...
  set.seed(1)
  # Return the graph (will plot at top level)
  Plotting.Utils::ilayout.addPlot(graph)
}
