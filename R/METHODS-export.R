#' Export a data object
#'
#' @param x The data object
#' @param path The path to a csv file in which to save the data
#' @rdname export
#' @export
export <- function(x,path) UseMethod("export")
#' @export
export.default <- function(x,path) {
  stop("Unable to export data for this class")
}
