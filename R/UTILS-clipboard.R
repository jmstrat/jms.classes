#' Get the contents of the clipboard
#'
#' @export
#' @rdname clipboard
clipboard_paste <- function() {
  if (Sys.info()["sysname"] == "Darwin") {
    file <- pipe("pbpaste")
    data <- scan(file, "character", sep="\n", quiet=TRUE)
    close(file)
  } else {
    data <- readLines("clipboard", warn=FALSE)
  }
  data
}

#' Copy data to the clipboard
#'
#' @export
#' @rdname clipboard
clipboard_copy <- function(string) {
  if (Sys.info()["sysname"] == "Darwin") {
    file <- pipe("pbcopy", "w")
    write(string, file)
    close(file)
  } else {
    write(string, "clipboard")
  }
}
