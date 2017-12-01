saveTable <- function(table,path) {
  parent <- parent.env(table)
  parent.env(table) <- emptyenv()
  make_lockfile(paste0(path,'.lock'))
  log.debug('Writing table %s to %s',table$.name,path)
  saveRDS(table,path)
  dg<-digest::digest(path, algo = "sha1", file = T)
  remove_lockfile(paste0(path,'.lock'))
  parent.env(table) <- parent
  dg
}

loadTable <- function(path) {
  make_lockfile(paste0(path,'.lock'))
  log.debug('Reading table from %s',path)
  table=readRDS(path)
  dg<-digest::digest(path, algo = "sha1", file = T)
  remove_lockfile(paste0(path,'.lock'))
  list(hash=dg,table=table)
}
