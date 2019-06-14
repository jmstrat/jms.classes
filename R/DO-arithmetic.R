#' Add a number to a data objects y values
#'
#' @param e1 The data object
#' @param e2 The number to add
#' @return The data object
#' @rdname arithmetic.jms
#' @export
`+.jms.data.object` <- function(e1, e2) {
  le2 <- ncol(e2)
  if (is.null(le2)) le2 <- length(e2)
  nr <- nrow(e1)
  ny <- length(ycol(e1))

  if (le2 == nr) {
    # Standard vector addition
    # Use defaults safely
  } else if (le2 > 1 && ny > 1) {
    if (le2 != ny) {
      warning("Unsupported length for addition vector, using defaults -- do not expect sensible results!")
    } else {
      d <- dim(e2)
      if (is.null(d)) {
        # Add a different value from each column
        e2 <- matrix(rep(e2, each=nrow(e1)), nrow(e1), ny)
      } else {
        e2 <- as.data.frame(e2)
      }
    }
  }
  e1[, ycol(e1)] <- .Primitive("+")(as.data.frame(e1)[, ycol(e1)], e2)
  e1
}

#' Subtract a number to a data objects y values
#'
#' @param e1 The data object
#' @param e2 The number to subtract
#' @return The data object
#' @rdname arithmetic.jms
#' @export
`-.jms.data.object` <- function(e1, e2) {
  le2 <- ncol(e2)
  if (is.null(le2)) le2 <- length(e2)
  nr <- nrow(e1)
  ny <- length(ycol(e1))

  if (le2 == nr) {
    # Standard vector subtraction
    # Use defaults safely
  } else if (le2 > 1 && ny > 1) {
    if (le2 != ny) {
      # Unknown intention -- fall back to defaults with warning
      warning("Unsupported length for subtraction vector, using defaults -- do not expect sensible results!")
    } else {
      d <- dim(e2)
      if (is.null(d)) {
        # Subtract a different value from each column
        e2 <- matrix(rep(e2, each=nrow(e1)), nrow(e1), ny)
      } else {
        e2 <- as.data.frame(e2)
      }
    }
  }
  # Never change the x column
  e1[, ycol(e1)] <- .Primitive("-")(as.data.frame(e1)[, ycol(e1)], e2)
  e1
}

#' Multiply a data objects y values by a number
#'
#' @param e1 The data object
#' @param e2 The number by which to multiply
#' @return The data object
#' @rdname arithmetic.jms
#' @export
`*.jms.data.object` <- function(e1, e2) {
  le2 <- ncol(e2)
  if (is.null(le2)) le2 <- length(e2)
  nr <- nrow(e1)
  ny <- length(ycol(e1))

  if (le2 == nr) {
    # Standard vector multiplication
    # Use defaults safely
  } else if (le2 > 1 && ny > 1) {
    if (le2 != ny) {
      warning("Unsupported length for multiplication vector, using defaults -- do not expect sensible results!")
    } else {
      d <- dim(e2)
      if (is.null(d)) {
        # Subtract a different value from each column
        e2 <- matrix(rep(e2, each=nrow(e1)), nrow(e1), ny)
      } else {
        e2 <- as.data.frame(e2)
      }
    }
  }
  e1[, ycol(e1)] <- .Primitive("*")(as.data.frame(e1)[, ycol(e1)], e2)
  e1
}

#' Divide a data objects y values by a number
#'
#' @param e1 The data object
#' @param e2 The number by which to divide
#' @return The data object
#' @rdname arithmetic.jms
#' @export
`/.jms.data.object` <- function(e1, e2) {
  le2 <- ncol(e2)
  if (is.null(le2)) le2 <- length(e2)
  nr <- nrow(e1)
  ny <- length(ycol(e1))

  if (le2 == nr) {
    # Standard vector division
    # Use defaults safely
  } else if (le2 > 1 && ny > 1) {
    if (le2 != ny) {
      warning("Unsupported length for division vector, using defaults -- do not expect sensible results!")
    } else {
      d <- dim(e2)
      if (is.null(d)) {
        # Subtract a different value from each column
        e2 <- matrix(rep(e2, each=nrow(e1)), nrow(e1), ny)
      } else {
        e2 <- as.data.frame(e2)
      }
    }
  }
  e1[, ycol(e1)] <- .Primitive("/")(as.data.frame(e1)[, ycol(e1)], e2)
  e1
}
