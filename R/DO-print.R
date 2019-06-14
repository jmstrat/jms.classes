#' @export
print.jms.data.object <- function(x, ..., digits=NULL, quote=FALSE, right=TRUE, row.names=TRUE) {
  n <- length(row.names(x))
  if (length(x) == 0L) {
    classes <- class(x)
    classes_ <- c()
    for (i in classes) {
      if (i == "jms.data.object") break
      classes_ <- append(classes_, i)
    }
    extendedby <- if (length(classes_)) gsub("\\.", " ", paste0("(extended by ", paste0(classes_, collapse=", "), ") ")) else ""
    cat(sprintf(ngettext(
      n, "JMS data object %swith 0 columns and %d row",
      "JMS data object %swith 0 columns and %d rows"
    ), extendedby, n), "\n",
    sep=""
    )
  }
  else if (n == 0L) {
    print.default(names(x), quote=FALSE)
    cat(gettext("<0 rows> (or 0-length row.names)\n"))
  }
  else {
    m <- as.matrix(format.data.frame(x,
      digits=digits,
      na.encode=FALSE
    ))
    if (!isTRUE(row.names)) {
      dimnames(m)[[1L]] <- if (identical(row.names, FALSE)) {
        rep.int("", n)
      } else {
        row.names
      }
    }
    print(m, ..., quote=quote, right=right)
  }
  invisible(x)
}
