#' @title (Install and) load packages.
#'
#' @description This function checks if each package that needs to be loaded has been installed, installs any that are not, and then loads all of the listed packages.
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
