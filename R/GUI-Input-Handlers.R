shinyInputConvertToMatrix <- function(x, session, inputname) {
  x$matrix <- do.call("cbind", x$matrix)
  x
}
