#' @title Generate circle data.
#'
#' @description This function generates data that can be used to draw circles in ggplot.
#' @param center Numeric vector for the x and y position of the center of the circle. Default is c(0, 0).
#' @param radius Radius of the circle. Default is 2.
#' @param npoints The number of data points to generate. Default is 100.
#' @export
#' @examples
#' \dontrun{
#' circle_data <- circle_fun(c(1,-1), 2.3, npoints = 100)
#' library("ggplot2")
#' ggplot(circle_data, aes(x,y)) + geom_path()
#' }
circle_fun <- function(center = c(0, 0),
                       radius = 2,
                       npoints = 100) {
  tt <- seq(0, 2 * pi, length.out = npoints)
  xx <- center[1] + radius * cos(tt)
  yy <- center[2] + radius * sin(tt)
  return(data.frame(x = xx, y = yy))
}
