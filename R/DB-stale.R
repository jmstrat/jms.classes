#' @export
is.stale.jms.database <- function(x) {
  path=paste0(x$.path,'/database')
  mt=file.info(path)$mtime
  if(is.na(mt)) mt=.POSIXct(0)
  if(x$.modTime<mt) {
    log.info('Database is stale')
    return(T)
  } else {
    log.info('Database is not stale')
    return(F)
  }
}
