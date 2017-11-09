#' Retain attributes upon subsetting
#' @export
`[.jms.data.object` <- function (x,i=T,j=T, ...) {
  r <- NextMethod("[")
  # Restore attributes:
  # Don't restore these
  special_attrs=c('class', 'comment', 'dim', 'dimnames', 'names', 'row.names', 'tsp')
  oldAtts=attributes(x)
  oldAtts=oldAtts[!names(oldAtts)%in%special_attrs]
  nOldAtts=names(oldAtts)
  attributes(r)[nOldAtts]<-oldAtts
  # Update class attributes:
  # Sometimes j is not set but exists() ?!?!?!
  j<-tryCatch({get('j')},error=function(e) TRUE)
  # Check we still have the x column after subsetting and whether its number has changed
  if('x_column'%in%nOldAtts) {
    if(is.numeric(j))
      attributes(r)[['x_column']]=which(sort(j)%in%oldAtts[['x_column']])
    else if(is.logical(j))
      attributes(r)[['x_column']]=which((1:ncol(x))[j]%in%oldAtts$x_column)
    else
      attributes(r)[['x_column']]=integer()
  }
  # Check which y columns we still have and get their new numbers
  if('y_column'%in%nOldAtts) {
    if(is.numeric(j))
      attributes(r)[['y_column']]=which(sort(j)%in%oldAtts[['y_column']])
    else if(is.logical(j))
      attributes(r)[['y_column']]=which((1:ncol(x))[j]%in%oldAtts$y_column)
    else
      attributes(r)[['y_column']]=integer()
  }
  # Check which y2 columns we still have and get their new numbers
  if('y2_column'%in%nOldAtts) {
    if(is.numeric(j))
      attributes(r)[['y2_column']]=which(sort(j)%in%oldAtts[['y2_column']])
    else if(is.logical(j))
      attributes(r)[['y2_column']]=which((1:ncol(x))[j]%in%oldAtts$y2_column)
    else
      attributes(r)[['y2_column']]=integer()
  }
  # Restore the class if the dataset is still 2D
  if(inherits(r,'data.frame')) class(r) <- c("jms.data.object","data.frame")
  return (r)
}
