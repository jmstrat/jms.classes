#' Export a data object
#'
#' @param x The data object
#' @param path The path to a csv file in which to save the data
#' @rdname export
#' @export
export <- function(x, path) UseMethod("export")
#' @export
export.default <- function(x, path) {
  stop("Unable to export data for this class")
}
#' @export
export.list <- function(x, path) {
  # Do we have an export method for every item in the list?
  if (any(as.character(.S3methods(export)) %in% paste0("export.", Reduce(intersect, lapply(x, class))))) {
    n <- names(x)
    fn <- tools::file_path_sans_ext(path)
    ext <- tools::file_ext(path)

    paths <- paste0(fn, "_", n, ".", ext)

    mapply(function(y, p) export(y, p), x, paths)
  } else {
    export.default(x, path)
  }
  return(invisible())
}
