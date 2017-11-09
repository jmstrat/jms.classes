#' @export
export.jms.data.object <- function(x,path) {
  atts=attributes(x)
  attNames=names(atts)
  attNames=attNames[!attNames %in% c("names","row.names","class",'y_type','x_type','x_column','y_column')]
  f <- file(path, "w")
  for(att in attNames) {
    if(inherits(atts[[att]],'data.frame')) {
      writeLines(paste0(att,':'),f)
      write.table(atts[[att]],f,sep=",",row.names =FALSE)
    } else
      writeLines(paste(att, atts[[att]],sep=','),f)
  }
  write.table(x,f,sep=",",row.names =FALSE)
  close(f)
}
