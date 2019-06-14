#' Read a table as a JMS data object
#'
#' @param ... parameters are passed to \code{\link{read.table}}
#' @return A JMS data object containing the data
#' @export
read.table.jms <- function(...) {
  df <- utils::read.table(...)
  as.jms.data.object(df)
}

#' Make a new JMS data object
#'
#' @details A \code{jms.data.object} is effectively just a \code{\link{data.frame}} that keeps track of which column
#' represents the x-axis and which columns represent the y-axes. This means that it can be easily plotted, and can
#' behave slightly more inteligently in certain circumstances:
#' \describe{
#'  \item{Arithmetic Operations}{i.e. \code{+}, \code{-}, \code{*}, \code{/} only apply to y columns by default}
#'  \item{Range Operations}{i.e. \code{\link{range}}, \code{\link{min}}, \code{\link{max}} only apply to y columns by default}
#'  \item{diff}{Calculates the difference between 2 different \code{jms.data.object}s using their respective \code{x} and \code{y} columns.}
#'  \item{Interactive Plotting}{Data objects can produce an interactive plot using \code{\link{iplot}}}
#'  \item{Exporting}{Data objects can be exported using \code{\link{export}}}
#' }
#' You can convert a \code{jms.data.object} into a \code{\link{data.frame}} using \code{\link{as.data.frame}}.
#'
#' @param ... parameters are passed to \code{\link{data.frame}}
#' @return A JMS data object
#' @seealso \code{\link{xcol}}, \code{\link{xlab}}, \code{\link{xscale}}
#' @export
jms.data.object <- function(...) {
  return(as.jms.data.object(data.frame(...)))
}

#' Check if an object is a jms.data.object
#'
#' @param x The object to be tested
#' @return TRUE / FALSE
#' @export
is.jms.data.object <- function(x) {
  return(inherits(x, "jms.data.object"))
}

#' Convert an object into a JMS data object
#'
#' @param x The object to be converted
#' @return The converted object
#' @export
as.jms.data.object <- function(x) UseMethod("as.jms.data.object")

#' @export
as.jms.data.object.default <- function(x) {
  stop("Unable to convert this class")
}

#' @export
as.jms.data.object.data.frame <- function(x) {
  attr(x, "class") <- c("jms.data.object", "data.frame")
  atts <- names(attributes(x))
  if (!"file_type" %in% atts) attr(x, "file_type") <- NULL
  if (!"data_type" %in% atts) attr(x, "data_type") <- NULL
  if (!"y_type" %in% atts) ylab(x) <- "Unknown"
  if (!"y2_type" %in% atts) y2lab(x) <- NA
  if (!"x_type" %in% atts) xlab(x) <- "Unknown"
  if (!"x_column" %in% atts && ncol(x) > 0) xcol(x) <- 1
  if (!"y_column" %in% atts && ncol(x) > 1) ycol(x) <- 2
  if (!"y2_column" %in% atts) y2col(x) <- NA_integer_
  return(x)
}

#' @export
as.jms.data.object.matrix <- function(x) {
  as.jms.data.object(as.data.frame(x))
}
