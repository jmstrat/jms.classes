#' @export
export.jms.data.object <- function(x, path) {
  log.info('Exporting data object to "%s"', path)
  atts <- attributes(x)
  attNames <- names(atts)
  attNames <- attNames[!attNames %in% c(
    "names", "row.names", "class",
    "y_type", "x_type", "x_column",
    "y_column", "y2_type", "y2_column"
  )]
  f <- file(path, "w")

  if (length(attNames)) {
    writeLines("==== Data Attributes ====", f)
  }

  for (att in attNames) {
    if (inherits(atts[[att]], "data.frame")) {
      writeLines(paste0(att, ":"), f)
      utils::write.table(atts[[att]], f, sep=",", row.names=FALSE)
    } else if (inherits(atts[[att]], "list")) {
      writeLines(paste0(att, ":"), f)
      ul <- unlist(atts[[att]], T, T)
      writeLines(paste("", names(ul), ul, sep=","), f)
    } else {
      writeLines(paste(att, atts[[att]], sep=","), f)
    }
  }

  if (length(attNames)) {
    writeLines("", f)
    writeLines("==== Data ====", f)
  }

  utils::write.table(x, f, sep=",", row.names=FALSE)
  close(f)
}
