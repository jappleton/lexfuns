#' @title A Stata-esque destring function for converting factor/character variables to numeric.
#'
#' @description This function converts character and factor variables to numeric, or leaves the variable as is if it is already numeric. If the variable is some other type, the function will throw an error and, if the script is being sourced, stop the script from running further.
#' @param x Input the variable to be converted.
#' @export
destring <- function(x) {
  if (is.character(x)) {
    as.numeric(x)
  }
  else if (is.factor(x)) {
    as.numeric(levels(x))[x]
  }
  else if (is.numeric(x)) {
    x
  }
  else {
    stop("could not convert to numeric")
  }
}
