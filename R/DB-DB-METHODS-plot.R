plotDatabase <- function(database) {
  load(database)
  path=database$.path
  entries=paste0(length(database$.table_names),' Tables')
  op <- graphics::par(mar = rep(0, 4),oma=rep(0, 4))
  graphics::plot.new()
  graphics::plot.window(c(0,1), c(0,1), xaxs = "i", yaxs = "i")
  graphics::rect(0,0.66,1,1,col=grDevices::rgb(240,117,15,maxColorValue = 255),border=NA)
  graphics::text(0.5,0.83,'Database')
  graphics::rect(0,0.33,1,0.66,col=grDevices::rgb(244,128,32,maxColorValue = 255),border=NA)
  graphics::text(0.5,0.49,path)
  graphics::rect(0,0,1,0.33,col=grDevices::rgb(240,149,55,maxColorValue = 255),border=NA)
  graphics::text(0.5,0.17,entries)

  graphics::par(op)
}
