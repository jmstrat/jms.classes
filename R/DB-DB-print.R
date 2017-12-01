#' @export
print.jms.database <- function (x,...) cat('jms.database with the following tables:',paste0(x$.table_names,collapse=', '))
