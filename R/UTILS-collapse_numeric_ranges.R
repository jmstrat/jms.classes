#' Collapse a numeric vector to a character vector of ranges
#'
#' @param x The numeric vector
#' @param min_collapse Minimum length of range before collapsing
#' @export
#' @examples
#' s <- c(1, 2, 3, 4, 8, 9, 14, 15, 16, 19, 21, 24, 25, 26)
#'
#' paste0(numeric_to_string_ranges(s), collapse=", ")
#' paste0(numeric_to_string_ranges(s, 3), collapse=", ")
#' paste0(numeric_to_string_ranges(s, 4), collapse=", ")
numeric_to_string_ranges <- function(x, min_collapse=2) {
  diffs <- c(1, diff(x))
  is_step <- diffs != 1
  range_list <- split(x, cumsum(is_step))
  unlist(lapply(
    range_list,
    function(r) if (length(r) < min_collapse) as.character(r) else paste0(r[1], "-", r[length(r)])
  ),
  use.names=FALSE
  )
}
