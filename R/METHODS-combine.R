#' Combine a list of data objects
#'
#' This function combines jms.data.objects
#' @param A \code{list} of jms.data.objects
#' @return A jms.data.object containing the data
#' @examples
#' combine(objects)
#' @export
combine <- function(objects,interpolate,maxPoints,rescale) UseMethod("combine")
#' @export
combine.default <- function(objects,interpolate,maxPoints,rescale) {
  stop("Unable to combine data for this class")
}
