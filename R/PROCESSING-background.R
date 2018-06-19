#' Create a background based on a vector of x values which lie on the baseline
#'
#' This function creates a spline background for the given data and x points
#' @param xy The data to create the background for (data.frame(x=..., y=...))
#' @param x_points X values which lie on the baseline
#' @param bkg_y_avg_points How many points either side of each x point to use to account for noise
#' @return A vector containing the y values for the background
#' @export
make_background <- function(xy, x_points, bkg_y_avg_points = 4) {
  bkg_x_points = vapply(x_points, function(x) which.min(abs(xy[,1] - x)), 1)
  l = length(xy[,1])
  r = range(bkg_x_points)
  if(r[[1]]<=0) bkg_x_points = bkg_x_points + 1 - r[[1]]
  if(r[[2]]>l) bkg_x_points = bkg_x_points + l - r[[2]]

  bkg_y = sapply(Map(seq, from = bkg_x_points-bkg_y_avg_points, to = bkg_x_points+bkg_y_avg_points),
                 function(i) mean(xy[i,2]))

  splinefun(x_points,y=bkg_y)(xy[,1])
}
