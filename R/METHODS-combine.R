#' Combine a list of data objects
#'
#' This function combines jms.data.objects
#' @param A \code{list} of jms.data.objects
#' @return A jms.data.object containing the data
#' @examples
#' combine(objects)
combine <- function(objects,interpolate) UseMethod("combine")
#' @export
combine.default <- function(objects,interpolate) {
  stop("Unable to combine data for this class")
}
