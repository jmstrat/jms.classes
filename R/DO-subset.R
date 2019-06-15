#' Retain attributes upon subsetting
#' @export
`[.jms.data.object` <- function(x, ...) {
  r <- NextMethod("[")
  # Restore attributes:
  # Don't restore these
  special_attrs <- c("class", "comment", "dim", "dimnames", "names", "row.names", "tsp")
  oldAtts <- attributes(x)
  oldAtts <- oldAtts[!names(oldAtts) %in% special_attrs]
  nOldAtts <- names(oldAtts)
  attributes(r)[nOldAtts] <- oldAtts

  new_cols <- match(colnames(r), colnames(x))

  # Check we still have the x column after subsetting and whether its number has changed
  if ("x_column" %in% nOldAtts) {
    attributes(r)[["x_column"]] <- which(new_cols %in% oldAtts[["x_column"]])
  }
  # Check which y columns we still have and get their new numbers
  if ("y_column" %in% nOldAtts) {
    attributes(r)[["y_column"]] <- which(new_cols %in% oldAtts[["y_column"]])
  }
  # Check which y2 columns we still have and get their new numbers
  if ("y2_column" %in% nOldAtts) {
    attributes(r)[["y2_column"]] <- which(new_cols %in% oldAtts[["y2_column"]])
  }
  # Check which scaled columns we still have and get their new numbers
  if (".scaled" %in% nOldAtts) {
    attributes(r)[[".scaled"]] <- which(new_cols %in% oldAtts[[".scaled"]])
  }
  # Restore the class if the dataset is still 2D
  if (inherits(r, "data.frame")) class(r) <- c("jms.data.object", "data.frame")
  return(r)
}


##### UPDATED 24/02/19 #####
##### OLD VERSION #####
# `[.jms.data.object` <- function (x,i=T,j=T, ...) {
#   r <- NextMethod("[")
#   # Restore attributes:
#   # Don't restore these
#   special_attrs=c('class', 'comment', 'dim', 'dimnames', 'names', 'row.names', 'tsp')
#   oldAtts=attributes(x)
#   oldAtts=oldAtts[!names(oldAtts)%in%special_attrs]
#   nOldAtts=names(oldAtts)
#   attributes(r)[nOldAtts]<-oldAtts
#   # Update class attributes:
#   # Sometimes j is not set but exists() ?!?!?!
#   j<-tryCatch({get('j')},error=function(e) TRUE)
#   # Check we still have the x column after subsetting and whether its number has changed
#   new_cols <- (1:ncol(x))[j]
#   if('x_column'%in%nOldAtts) {
#     attributes(r)[['x_column']]=which(new_cols%in%oldAtts[['x_column']])
#   }
#   # Check which y columns we still have and get their new numbers
#   if('y_column'%in%nOldAtts) {
#     attributes(r)[['y_column']]=which(new_cols%in%oldAtts[['y_column']])
#   }
#   # Check which y2 columns we still have and get their new numbers
#   if('y2_column'%in%nOldAtts) {
#     if(is.numeric(j))
#       attributes(r)[['y2_column']]=which(new_cols%in%oldAtts[['y2_column']])
#   }
#   # Restore the class if the dataset is still 2D
#   if(inherits(r,'data.frame')) class(r) <- c("jms.data.object","data.frame")
#   return (r)
# }
#
