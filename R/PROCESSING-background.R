#' Create a background based on a vector of x values which lie on the baseline
#'
#' This function creates a spline background for the given data and x points
#' @param xy The data to create the background for (data.frame(x=..., y=...))
#' @param x_points X values which lie on the baseline
#' @param bkg_y_avg_points How many points either side of each x point to use to account for noise
#' @param returnFunc Returns a splinefun rather than a vector of y values
#' @return A vector containing the y values for the background
#' @export
make_background <- function(xy, x_points, bkg_y_avg_points = 4, returnFunc = FALSE) {
  bkg_x_points = vapply(x_points, function(x) which.min(abs(xy[,1] - x)), 1)
  l = length(xy[,1])
  r = range(bkg_x_points)
  if(r[[1]]<=0) bkg_x_points = bkg_x_points + 1 - r[[1]]
  if(r[[2]]>l) bkg_x_points = bkg_x_points + l - r[[2]]

  bkg_y = sapply(Map(seq, from = bkg_x_points-bkg_y_avg_points, to = bkg_x_points+bkg_y_avg_points),
                 function(i) mean(xy[i,2]))

  fun <- splinefun(x_points,y=bkg_y)
  if(returnFunc) return(fun)

  fun(xy[,1])
}


#' Create backgrounds based on a list of vectors of x values which lie on the baseline
#'
#' This function creates a spline background for the given data and x points
#' @param data The data to create the background for (data.frame(x=..., y=..., y2=...))
#' @param baseline_parameters list of length equal to the number of y columns.
#'                            A background will be created for each element of the list using
#'                            \code{\link{make_background}}.
#' @return A \code{jms.data.object} containing the backgrounds that can be subtracted from the data
#' @export
make_backgrounds <- function(data, baseline_parameters, bkg_y_avg_points = 4) {
  if(is.null(baseline_parameters)) return(0)
  log.info('Making baselines for data')
  x <- xcol(data)[[1]]
  nr <- nrow(data)
  bkgs <- mapply(function(a,b) if(is.null(b)) rep_len(0, nr) else make_background(data[,c(x,a)], b, bkg_y_avg_points = bkg_y_avg_points), ycol(data), baseline_parameters)
  as.jms.data.object(bkgs)
}

#' Expand a table of baseline parameters for use with \code{\link{make_backgrounds}}
#'
#' @param scans_and_points data.frame with the 1st column containing lists of x points, the second column containing lists of scan numbers
#' @return A list that can be used as the baseline_parameters argument to \code{\link{make_backgrounds}}
#' @export
expand_baseline_parameters <- function(scans_and_points, nscans) {
  scan_list <- scans_and_points[, 2, drop=FALSE]
  if(!length(scan_list)) return()
  where <- sapply(scan_list, function(x) 1:nscans %in% x)

  lapply(1:nscans, function(i) {
    idx <- which(where[i,])
    if(length(idx) == 0) {
      NULL
    } else {
      scans_and_points[[idx[[1]],1]]
    }
  })
}
