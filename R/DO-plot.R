set_default_arg <- function(name, args, expr, env=parent.frame()) {
  if (!name %in% names(args) || any(is.null(args[[name]]))) {
    env[[deparse(substitute(args))]][[name]] <- eval(substitute(expr), envir=env)
  }
}

#' Plot a data object
#'
#' This function plots a data object
#' @param x The object to plot
#' @param offset For 2D data objects, the fraction of the yrange to offset the data
#' @param ... Further parameters are passed to \code{\link[Plotting.Utils]{pretty_plot}} or the \code{\link{lines}} method for \code{class(x)}.
#' @details
#' If not specified, \code{xlim} will set to \code{range(x[,\link{xcol}(x)]}, \code{ylim} will be based on \code{range(x)}
#' and \code{offset}, y2lim will be based on \code{range(x[,\link{y2col}(x)]}.\cr\cr
#'
#' \code{xlab} will be set to \code{\link{xlab}}(x), \code{ylab} to \code{\link{ylab}}(x), and
#' \code{y2lab} to \code{\link{y2lab}}(x).\cr\cr
#'
#' Data are scalled by \code{\link{xscale}}(x), \code{\link{yscale}}(x), and \code{\link{y2scale}}(x).
#' @examples
#' plot(data)
#' @export
plot.jms.data.object <- function(x, offset=NULL, ...) {
  log.debug("Plotting a %s", class(x)[[1]])

  ly <- length(ycol(x))
  lo <- length(offset)

  # To allow unit conversions etc.
  x <- get_scaled(x)

  # Apply the offsets
  if (ly > 1) {
    if (lo == 0) {
      offset <- 1 / sqrt(length(ycol(x)) - 1) * range(x)[[2]]
    }
    else if (lo == ly) {
      offset <- offset * range(x)[[2]]
    } else if (lo == 1) {
      offset <- offset * seq(0, length(ycol(x)) - 1, 1) * range(x)[[2]]
    } else {
      warning("Unsupported length for offset, using 1st value for all offsets")
      offset <- offset[[1]] * seq(0, length(ycol(x)) - 1, 1) * range(x)[[2]]
    }
    x <- x + offset
  } else {
    offset <- 0
  }

  args <- list(...)

  set_default_arg("xlim", args, {
    range(x[, xcol(x)][is.finite(x[, xcol(x)])])
  })

  set_default_arg("ylim", args, {
    grDevices::extendrange(r=range(x), 0.04)
  })

  set_default_arg("axes", args, {
    c(1, 2)
  })

  set_default_arg("y2lim", args, {
    if (!all(is.na(y2col(x)))) {
      range(x[, y2col(x)], na.rm=T)
    } else if (4 %in% args$axes) {
      args$ylim
    }
  })

  set_default_arg("xlab", args, {
    xlab(x)
  })

  set_default_arg("ylab", args, {
    ylab(x)
  })

  set_default_arg("y2lab", args, {
    y2lab(x)
  })

  log.debug("Plotting arguments: [%s]", paste(names(args), "=", args, collapse=", ", sep=""))

  plot_args <- args[names(args) %in% names(c(
    formals(graphics::axis),
    formals(Plotting.Utils::pretty_plot),
    formals(Plotting.Utils::pretty_axes),
    par()
  ))]

  plot_args <- plot_args[!names(plot_args) %in% c("col", "lwd", "lty")]
  # Draw the axes
  do.call(Plotting.Utils::pretty_plot, plot_args)

  lines_args <- args[!names(args) %in% iplotArgBlacklist]
  lines_args <- lines_args[!names(lines_args) %in% names(plot_args)]
  lines_args <- append(list(x=x), lines_args)
  # Draw the data
  do.call("lines", lines_args)
  invisible(offset)
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
lines.jms.data.object <- function(x, col=par("col"), type="l", y2=TRUE, cex.points=par("cex"), ...) {
  log.debug("Drawing lines for a %s", class(x)[[1]])

  x_data <- x[, xcol(x)]
  y_cols <- ycol(x)
  y_cols <- y_cols[!is.na(y_cols)]
  y_df <- if (length(y_cols) == 0) c() else x[, y_cols]
  y2_cols <- y2col(x)
  y2_cols <- y2_cols[!is.na(y2_cols)]
  y2_df <- if (length(y2_cols) == 0) c() else x[, y2_cols]

  if (length(col) == 1 && is.function(col)) {
    col_all <- col(length(y_cols))
  } else {
    col_all <- if (y2) expand_args(1:(length(y_cols) + length(y2_cols)), col)[[2]] else expand_args(1:length(y_cols), col)[[2]]
  }
  type_all <- if (y2) expand_args(1:(length(y_cols) + length(y2_cols)), type)[[2]] else expand_args(1:length(y_cols), type)[[2]]
  cex <- cex.points
  cex.points <- NULL
  y2_ <- y2
  y2 <- NULL

  if (length(y_cols) == 1) {
    x <- data.frame(x=x_data, y=y_df)
    NextMethod(type=type_all[[1]], cex=cex)
  } else if (length(y_cols) > 1) {
    for (i in 1:length(y_cols)) {
      x <- data.frame(x=x_data, y=y_df[, i])
      col <- col_all[[i]]
      NextMethod(type=type_all[[i]], cex=cex)
    }
  }

  if (y2_) {
    if (length(y2_cols) == 1) {
      x <- data.frame(x=x_data, y=Plotting.Utils::rescaleOntoY2(y2_df))
      col <- col_all[[1 + length(y_cols)]]
      NextMethod(type=type_all[[1 + length(y_cols)]], cex=cex)
    } else if (length(y2_cols) > 1) {
      for (i in 1:length(y2_cols)) {
        x <- data.frame(x=x_data, y=Plotting.Utils::rescaleOntoY2(y_df[, i]))
        col <- col_all[[i + length(y_cols)]]
        NextMethod(type=type_all[[i + length(y_cols)]], cex=cex)
      }
    }
  }
}
