#' @title Convert radians to degrees.
#'
#' @description This function converts radians to degrees. It is commonly needed in code that we use to graph circles.
#' @param rad Input the radian that is to be converted into degrees.
#' @export
rad2deg <-
  function(rad) {
    (rad * 180) / (pi)
  }
