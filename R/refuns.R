#' @title Calculate age in years.
#'
#' @description This function calculates age in years as of a certain date, correctly accounting for leap years. You could also use it to calculate years between any two dates.
#' @param birth_dt Input birthdate as a string with formatting YYYY/MM/DD (e.g., "1981/02/25")
#' @param as_of_dt Input date at which age is to be calculated as a string with formatting YYYY/MM/DD (e.g., "2019/05/21")
#' @export
#' @examples
#' \dontrun{
#' born <- "1981/02/25"
#' today <- Sys.Date()
#' calc_age_in_yrs(born, today)
#' }
calc_age_in_yrs <- function(birth_dt, as_of_dt) {
  x_lt = as.POSIXlt(birth_dt)
  y_lt = as.POSIXlt(as_of_dt)

  age = y_lt$year - x_lt$year

  ifelse(y_lt$mon < x_lt$mon |
           (y_lt$mon == x_lt$mon & y_lt$mday < x_lt$mday),
         age - 1, age)
}






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
#' ggplot(dat,aes(x,y)) + geom_path()
#' }
circle_fun <- function(center = c(0, 0),
                      radius = 2,
                      npoints = 100) {
  tt <- seq(0, 2 * pi, length.out = npoints)
  xx <- center[1] + radius * cos(tt)
  yy <- center[2] + radius * sin(tt)
  return(data.frame(x = xx, y = yy))
}






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







#' @title Detach all packages.
#'
#' @description This function detaches as many aspects of installed packages from the working environment as is possible without a restart of the R session. Note that in some packages, it is not possible to fully detach them without restarting R.
#' @export
detach_all_pkgs <- function() {
  basic.packages <-
    c(
      "package:stats",
      "package:graphics",
      "package:grDevices",
      "package:utils",
      "package:datasets",
      "package:methods",
      "package:base"
    )
  package.list <-
    search()[ifelse(unlist(gregexpr("package:", search())) == 1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list) > 0)
    for (package in package.list)
      detach(package, character.only = TRUE)
}






#' @title (Install and) load packages.
#'
#' @description This function checks if each package that needs to be loaded has been installed installed, installs any that are not, and then loads all of the listed packages.
#' @param pkgs_needed Input a character vector of the packages you want to install/load. Can be a single package (e.g., "dplyr") or multiple (c("dplyr", "tidyr")).
#' @export
#' @examples
#' \dontrun{
#' pkgs_needed <- c("tidyverse", "zoo", "openxlsx")
#' install_and_load_pkgs(pkgs_needed)
#' }
install_and_load_pkgs <- function(pkgs_needed) {
  new_pkg <-
    pkgs_needed[!(pkgs_needed %in% installed.packages()[, "Package"])]
  if (length(new_pkg))
    install.packages(new_pkg, dependencies = TRUE)
  sapply(pkgs_needed, library, character.only = TRUE)
}






#' @title A Stata-esque function for asserting an id variable is unique.
#'
#' @description Based on the Stata function of the same name, this function asserts whether rows of a dataframe are unique, and if they are not, it will throw an error and, when sourced, will stop the sourced script code from running any further.This function converts character and factor variables to numeric, or leaves the variable as is if it is already numeric. If the variable is some other type, the function will throw an error and, if the script is being sourced, stop the script from running further.
#' @param x Input the ID to be checked for duplicates. Can be a single variable, or multiple.
#' @export
isid <- function(x) {
  if (sum(duplicated(x)) > 0) stop('Nope. Duplicates.')
  if (sum(duplicated(x)) == 0) return('Yep!')
  stopifnot(sum(duplicated(x)) == 0)
}






#' @title Convert radians to degrees.
#'
#' @description This function converts radians to degrees. It is commonly needed in code that we use to graph circles.
#' @param rad Input the radian that is to be converted into degrees.
#' @export
rad2deg <-
  function(rad) {
    (rad * 180) / (pi)
  }






#' @title Round like Excel.
#'
#' @description This function rounds numbers in the same way that Excel does:
#'
#' Rounds up the number line when values are positive and trailing value is 5-9.
#'
#' Rounds down the number line when values are negative and trailing value is 5-9.
#'
#' If a trailing value is 5 exactly, always rounds in the same direction (i.e., up the number
#' line when value is positive, down the number line when value is negative). This is unlike
#' R which uses a more statistically valid but less widely accepted rounding rule of randomly
#' rounding half of the exact 5s up and half of the exact 5s down.
#'
#' Solves floating point rounding errors that seem to occur at a rate of about 1/200 or so,
#' in which a value that looks like an exact 5 (e.g., 0.5) is stored with some "decimal dust"
#' (e.g., looks like 0.5 but is really 0.4999999999999999) because, long story short,
#' computers are weird.
#'
#' Would not solve bit flips caused by cosmic rays, though. Which is a very rare, but real thing. Look it up.
#' @param val Input the value to be rounded.
#' @param n_dec Input the number of decimal places to round to (e.g., 0 = round to integer, 1 = round to the tenths place, 2 = round to the hundreths place, etc.).
#' @export
rnd_like_xl = function(val, n_dec) {
  ifelse(val >= 0,
         round(val + 0.000000001, n_dec),
         round(val - 0.000000001, n_dec))
}
