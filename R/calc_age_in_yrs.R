#' @title Calculate age in years.
#'
#' @description This function calculates age in years as of a certain date, correctly accounting for leap years. You could also use it to calculate years between any two dates.
#' @param birth_dt Input birthdate as a string with formatting YYYY/MM/DD (e.g., "1981/02/25")
#' @param as_of_dt Input date at which age is to be calculated as a string with formatting YYYY/MM/DD (e.g., "2019/05/21")
#' @export
#' @examples
#' \dontrun{
#' born <- "1981/02/25"
#' yesterday <- Sys.Date() - 1
#' calc_age_in_yrs(born, yesterday)
#' }
calc_age_in_yrs <- function(birth_dt, as_of_dt) {
  x_lt = as.POSIXlt(birth_dt)
  y_lt = as.POSIXlt(as_of_dt)

  age = y_lt$year - x_lt$year

  ifelse(y_lt$mon < x_lt$mon |
           (y_lt$mon == x_lt$mon & y_lt$mday < x_lt$mday),
         age - 1, age)
}
