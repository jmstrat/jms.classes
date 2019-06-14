#' match the items in a list to a vector of names
#'
#' match.names returns a list with items ordered and names as defined by another vector
#'
#' @param names vector of names to be expected from list
#' @param list list of values to be matched with names
#' @return list with items ordered and names as defined by another vector
#' @export
match.names <- function(names, list) {
  listnames <- names(list)
  l1 <- length(names)
  l2 <- length(list)

  # List is unnamed
  if (!length(listnames)) {
    if (l1 < l2) {
      warning("Names vector is shorter than list, truncating list to fit")
      list <- list[1:l1]
    } else if (l1 > l2) {
      warning("Names vector is longer than list, truncating names to fit")
      names <- names[1:l2]
    }
    names(list) <- names
    return(list)
  }
  if (any(!(listnames %in% names || listnames == ""))) stop("Named list does not match expected names")
  namedList <- list[names]
  ina <- is.na(names(namedList))
  if (any(ina)) {
    # List is partially named
    missingNames <- names[!names %in% listnames]
    missingValues <- list[!listnames %in% names]
    if (l1 < l2) {
      warning("Names vector is shorter than list, truncating list to fit")
      gtl1 <- rep_len(FALSE, l2)
      gtl1[(l1 + 1):l2] <- TRUE
      namedList[ina && gtl1] <- NULL
      missingValues <- missingValues[1:(length(which(ina)))]
    } else if (l1 > l2) {
      warning("Names vector is longer than list, truncating names to fit")
      missingNames <- missingNames[1:(length(which(ina)))]
      missingValues[1:(l1 - l2)] <- vector("list", l1 - l2)
    }
    if (!all(names[(length(missingNames) + 1):l1] == missingNames)) {
      warning("List is unordered with missing names, assuming unnamed items are ordered")
    }
    names(namedList)[ina] <- missingNames
    namedList[ina] <- missingValues
    return(Filter(Negate(is.null), namedList))
  } else {
    if (l1 < l2) {
      warning("Names vector is shorter than list, truncating list to fit")
    }
    # List is fully named and all names match
    return(namedList)
  }
}
