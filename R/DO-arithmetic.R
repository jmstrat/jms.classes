do_arith <- function(e1, e2, prim_func) {
  if(is.jms.data.object(e2)) {
    e2 <- as.data.frame(e2[,ycol(e2)])
  }

  le2 <- ncol(e2)
  if (is.null(le2)) le2 <- length(e2)
  nr <- nrow(e1)
  yc <- ycol(e1)
  ny <- length(yc)

  if (le2 == nr) {
    # Standard vector arithmetic
    # Use defaults safely
  } else if (le2 > 1 && ny > 1) {
    if (le2 != ny) {
      # Unknown intention -- fall back to defaults with warning
      warning("Unsupported length for e2 vector, using defaults -- do not expect sensible results!")
    } else {
      d <- dim(e2)
      if (is.null(d)) {
        # Use a different value from each column
        e2 <- matrix(rep(e2, each=nrow(e1)), nrow(e1), ny)
      } else {
        e2 <- as.data.frame(e2)
      }
    }
  }
  # Never change the x column
  e1[, yc] <- .Primitive(prim_func)(as.data.frame(e1)[, yc], e2)
  e1
}

#' Data object arithmetic
#'
#' Perform arithmetic operations on the y columns for a data object.
#' \code{e2} can be:
#' \describe{
#'  \item{A single number}{The resulting data object has this number added / subtracted / ... to every y value}
#'  \item{A vector of length \code{nrow(e1)}}{e2 is added / subtracted / ... to each y column of e1}
#'  \item{A vector of length \code{length(ycol(e1))}}{Each y column has its corresponding value in e2 added / subtracted / ... to it}
#'  \item{A matrix of dimensions \code{nrow(e1) by length(ycol(e1))}}{Standard matrix addition / subtraction / ...}
#' }
#'
#' @param e1 The data object
#' @param e2 The number(s) to add, subtract, multiply, or divide
#' @return The data object
#' @rdname arithmetic.jms
#' @export
`+.jms.data.object` <- function(e1, e2) {
  do_arith(e1, e2, "+")
}

#' @rdname arithmetic.jms
#' @export
`-.jms.data.object` <- function(e1, e2) {
  do_arith(e1, e2, "-")
}

#' @rdname arithmetic.jms
#' @export
`*.jms.data.object` <- function(e1, e2) {
  do_arith(e1, e2, "*")
}

#' @rdname arithmetic.jms
#' @export
`/.jms.data.object` <- function(e1, e2) {
  do_arith(e1, e2, "/")
}
