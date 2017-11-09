#' Make an interactive plot for a data object
#'
#' @rdname iPlot
#' @export
iplot <- function(...) UseMethod("iplot")
#' @rdname iPlot
#' @export
iplot.default <- function(...) {
  stop("Unable to make an interactive plot for this class")
}
