#' Get the cannonical path to a file
#'
#' @param path Path to the file
#'
#' @export
#' @rdname paths
cannonicalPath <- function(path) {
  file.path(normalizePath(dirname(path), "/", mustWork=FALSE), basename(path))
}

#' Get the cannonical path to a file using data on the clipboard
#'
#' @export
#' @rdname paths
clipboard_to_path <- function() {
  cannonicalPath(clipboard_paste())
}
