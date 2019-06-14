#' Calculate Difference
#'
#' This function calculates the difference between two data objects
#' @param x The 1st data object
#' @param x2 The 2nd data object
#' @return
#' A data object representing the difference
#' @examples
#' diff(x, x2)
#' @export
diff.jms.data.object <- function(x, x2) {
  if (!(is.jms.data.object(x) && is.jms.data.object(x2))) stop("Data must be a jms.data.object")
  x_col <- xcol(x)
  y_cols <- ycol(x)

  x_col2 <- xcol(x2)
  y_cols2 <- ycol(x2)


  if (length(x_col) != 1 || length(x_col2) != 1) stop("Cannot process this data type (unknown x)")
  if (length(y_cols) != length(y_cols2)) {
    warning("Data has differing number of y columns, only using 1st")
    y_cols <- y_cols[[1]]
    y_cols2 <- y_cols2[[1]]
  }

  calc_diff <- function(x1, y1, x2, y2) {
    dat2_approxfun <- stats::approxfun(x2, y2)
    xdiff <- x1[x1 <= max(x2) & x1 >= min(x2)]
    y1 <- y1[x1 <= max(x2) & x1 >= min(x2)]

    ydiff <- rep_len(NA, length(y1))
    y2 <- rep_len(NA, length(y1))
    ydiff[x1 <= max(x2) & x1 >= min(x2)] <- y1 - dat2_approxfun(xdiff)
    y2[x1 <= max(x2) & x1 >= min(x2)] <- dat2_approxfun(xdiff)
    data.frame(y2=y2, ydiff=ydiff)
  }
  diff <- x
  diff[, 2:ncol(diff)] <- NULL
  xcol(diff) <- 1
  if (length(y_cols) > 1) {
    for (i in 1:length(y_cols)) {
      d <- calc_diff(x[, x_col], x[, y_cols[[i]]], x2[, x_col], x2[, y_cols2[[i]]])
      diff[paste0("y", i)] <- d$y2
      diff[paste0("diff.", i)] <- d$ydiff
    }
    ycol(diff) <- seq(3, (length(y_cols) * 2 + 1), 2)
  } else if (length(y_cols) == 1) {
    d <- calc_diff(x[, x_col], x[, y_cols], x2[, x_col], x2[, y_cols2])
    diff["y"] <- d$y2
    diff["diff"] <- d$ydiff
    ycol(diff) <- 3
  } else {
    stop("Unknown y data")
  }
  diff
}
