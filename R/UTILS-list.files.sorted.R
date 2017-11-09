#' list.files with sorting
#'
#' @param ... Parameters passed to \code{\link{list.files}}
#' @section Sorting:
#' Natural sorting will take a file name of the sort <string><number><string>.<extension> and sort based on the number in a natural manner that does not depend on the number of digits in that number.
#' i.e 10 would come after 9 in a natural sort, but after 1 using the default sorting method
#' @export
list.files.sorted <- function(...) {
  files=list.files(...)
  files[order(as.numeric(sub("^[^0-9]*([0-9]*).*", "\\1", basename(files))))]
}
