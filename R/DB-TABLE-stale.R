#' @export
is.stale.jms.database.table <- function(x) {
  path<-attr(x,'.path')
  if(is.null(path)) return(FALSE)
  mt=file.info(path)$mtime
  if(is.na(mt)) mt=.POSIXct(0)
  attr(x,'.modTime')<mt
}
