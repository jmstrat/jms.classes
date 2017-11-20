#' @export
print.jms.database.table <- function (x, ..., digits = NULL, quote = FALSE, right = TRUE, row.names = TRUE) {
  n <- length(row.names(x$.table))
  if (length(x$.table) == 0L) {
    cat(sprintf(ngettext(n, "jms database table with 0 columns and %d row",
                         "jms database table with 0 columns and %d rows"),n), "\n",
        sep = "")
  }
  else if (n == 0L) {
    print.default(names(x$.table), quote = FALSE)
    cat(gettext("<0 rows> (or 0-length row.names)\n"))
  }
  else {
    m <- as.matrix(format(x$.table, digits = digits,na.encode = FALSE))
    if (!isTRUE(row.names))
      dimnames(m)[[1L]] <- if (identical(row.names, FALSE))
        rep.int("", n)
    else row.names
    print(m, ..., quote = quote, right = right)
  }
  invisible(x$.table)
}
