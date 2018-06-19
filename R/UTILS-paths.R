#' Get the cannonical path to a file
#'
#' @param path Path to the file
#'
#' @export
#' @rdname paths
cannonicalPath <- function(path) {
  file.path(normalizePath(dirname(path), '/', mustWork = FALSE),basename(path))
}

#' Get the cannonical path to a file using data on the clipboard
#'
#' @export
#' @rdname paths
clipboard_to_path <- function() {
  if(Sys.info()["sysname"] == "Darwin") {
    file = pipe("pbpaste")
    data = scan(file, 'character', sep='\n', quiet=TRUE)
    close(file)
  } else {
    data = readLines("clipboard", warn=FALSE)
  }
  cannonicalPath(data)
}
