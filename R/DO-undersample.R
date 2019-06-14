#' Undersamples a large data object to speed up processing / plotting etc.
#'
#' @param data The data object
#' @param n For every n points, only the first will be taken
#' @return The undersampled data object
#' @export
undersample <- function(data, n) {
  if (n <= 1) {
    return(data)
  }
  log.info("Undersampling data object by factor %s", n)
  data[c(T, rep_len(F, n - 1)), ]
}
