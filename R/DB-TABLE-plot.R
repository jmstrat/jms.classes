#' @export
plot.jms.database.table <- function(x, ...) {
  name <- x$.name
  table <- x$.table
  name <- paste(toupper(substr(name, 1, 1)), substr(name, 2, nchar(name)), " Table", sep="")
  entries <- paste0(nrow(table), " Entries")
  params <- paste0(nrow(table) * ncol(table), " Parameters")
  op <- graphics::par(mar=rep(0, 4), oma=rep(0, 4))
  graphics::plot.new()
  graphics::plot.window(c(0, 1), c(0, 1), xaxs="i", yaxs="i")
  graphics::rect(0, 0.66, 1, 1, col=grDevices::rgb(0.1176, 0.6784, 0.9922, 0.5), border=NA)
  graphics::text(0.5, 0.83, name)
  graphics::rect(0, 0.33, 1, 0.66, col=grDevices::rgb(0.1412, 0.8039, 0.9961, 0.5), border=NA)
  graphics::text(0.5, 0.49, entries)
  graphics::rect(0, 0, 1, 0.33, col=grDevices::rgb(0.1490, 0.8627, 0.9961, 0.5), border=NA)
  graphics::text(0.5, 0.17, params)

  graphics::par(op)
}
