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
