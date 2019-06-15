#' @export
xscale.jms.data.object <- function(x) {
  xscale <- attr(x, "x_scale")
  if (is.null(xscale)) xscale <- 1
  xscale
}
#' @export
`xscale<-.jms.data.object` <- function(x, value) {
  attr(x, "x_scale") <- value
  x
}
#' @export
yscale.jms.data.object <- function(x) {
  yscale <- attr(x, "y_scale")
  if (is.null(yscale)) yscale <- 1
  yscale
}
#' @export
`yscale<-.jms.data.object` <- function(x, value) {
  attr(x, "y_scale") <- value
  x
}
#' @export
y2scale.jms.data.object <- function(x) {
  y2scale <- attr(x, "y2_scale")
  if (is.null(y2scale)) y2scale <- 1
  y2scale
}
#' @export
`y2scale<-.jms.data.object` <- function(x, value) {
  attr(x, "y2_scale") <- value
  x
}

get_scaled <- function(x) {
  isScaled <- attr(x, ".scaled")
  if (is.null(isScaled)) {
    isScaled <- c()
  }

  xc <- xcol(x)
  yc <- ycol(x)
  y2c <- y2col(x)

  xc <- xc[!is.na(xc) & !xc %in% isScaled]
  yc <- yc[!is.na(yc) & !yc %in% isScaled]
  y2c <- y2c[!is.na(y2c) & !y2c %in% isScaled]

  scaled <- c(xc, yc, y2c)
  scaled <- scaled[!is.na(scaled)]

  if (length(scaled) == 0) {
    return(x)
  }

  jms.classes::log.debug("Applying scale factors to data object")
  if (length(xc) > 0) x[, xc] <- x[, xc] * xscale(x)
  if (length(yc) > 0) x[, yc] <- x[, yc] * yscale(x)
  if (length(y2c) > 0) x[, y2c] <- x[, y2c] * y2scale(x)

  # Store the list of columns we have scaled
  scaled <- c(isScaled, scaled)

  jms.classes::log.debug("All scaled columns: [%s]", paste0(scaled, collapse=", "))
  attr(x, ".scaled") <- scaled
  x
}
