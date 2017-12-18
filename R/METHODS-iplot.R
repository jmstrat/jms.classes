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

#' @rdname iPlot
#' @export
iplotOutput <- function(id) {
  shiny::uiOutput(id,style='height:400px')
}

#' @rdname iPlot
#' @export
renderIPlot <- function(expr, env = parent.frame(), quoted = FALSE) {
  if(!quoted) expr <- substitute(expr)
  shiny::renderUI(expr,env=env,quoted=TRUE, outputArgs = list())
}
