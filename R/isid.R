#' @title A Stata-esque function for asserting an id variable is unique.
#'
#' @description Based on the Stata function of the same name, this function asserts whether rows of a dataframe are unique, and if they are not, it will throw an error and, when sourced, will stop the sourced script code from running any further.
#' @param x Input the ID to be checked for duplicates. Can be a single variable, or multiple.
#' @export
#' #' @examples
#' \dontrun{
#' # Works well with dplyr::select, especially when id consists of multiple columns
#' library(dplyr)
#' fake_data <- data.frame(x = c(1, 1, 2, 2, 3, 3), y = c('A', 'B', 'A', 'B', 'A', 'B'), z = c(47, 32, 64, 64, 42, 47))
#' isid(fake_data %>% select(x))
#' isid(fake_data %>% select(x, y))
#' }
isid <- function(x) {
  if (sum(duplicated(x)) > 0) stop('Nope. There are duplicates in this id.')
  if (sum(duplicated(x)) == 0) return('Yep! This is a unique id.')
  stopifnot(sum(duplicated(x)) == 0)
}
