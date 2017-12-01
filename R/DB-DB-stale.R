#' @export
is.stale.jms.database <- function(x) {
  path=paste0(x$.path,'/database')
  hash=digest::digest(path, algo = "sha1", file = T)
  if(x$.hash!=hash) {
    log.info('Database is stale')
    return(T)
  } else {
    log.info('Database is not stale')
    return(F)
  }
}
