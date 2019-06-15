#' Calculate Range
#'
#' This function calculates the range of a data object
#' @param x The data object
#' @return
#' The range
#' @examples
#' range(data)
#' @export
range.jms.data.object <- function(x, offset, ...) {
  if (!is.jms.data.object(x)) stop("x must be a jms.data.object")
  yc <- tryCatch(ycol(x), error=function(e) NULL)
  x <- as.matrix(x)
  if (!missing(offset)) {
    ym <- max(x[, yc])
    ny <- length(yc)
    nx <- nrow(x)
    offsets <- matrix(rep.int(offset * (0:(ny - 1)) * ym, nx), nx, ny, T)
    x[, yc] <- .Primitive("+")(x[, yc], offsets)
  }
  if (length(yc) > 0) {
    as.numeric(range.default(x[, yc], na.rm=TRUE))
  } else {
    as.numeric(range.default(x, na.rm=TRUE))
  }
}


#' Calculate Min
#'
#' This function calculates the minimum y value of a data object
#' @param x The data object
#' @return
#' The minimum y value
#' @examples
#' min(data)
#' @export
min.jms.data.object <- function(x, ...) {
  if (!is.jms.data.object(x)) stop("x must be a jms.data.object")
  yc <- tryCatch(ycol(x), error=function(e) NULL)
  x <- as.matrix(x)
  if (length(yc) > 0) {
    as.numeric(.Primitive("min")(x[, yc], na.rm=TRUE))
  } else {
    as.numeric(.Primitive("min")(x, na.rm=TRUE))
  }
}

#' Calculate Max
#'
#' This function calculates the minimum y value of a data object
#' @param x The data object
#' @return
#' The maximum y value
#' @examples
#' max(data)
#' @export
max.jms.data.object <- function(x, ...) {
  if (!is.jms.data.object(x)) stop("x must be a jms.data.object")
  yc <- tryCatch(ycol(x), error=function(e) NULL)
  x <- as.matrix(x)
  if (length(yc) > 0) {
    as.numeric(.Primitive("max")(x[, yc], na.rm=TRUE))
  } else {
    as.numeric(.Primitive("max")(x, na.rm=TRUE))
  }
}
