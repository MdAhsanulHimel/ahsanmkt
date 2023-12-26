
#' @export
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.dplyr <- list(
    dplyr.show_progress = TRUE
  )
  toset <- !(names(op.dplyr) %in% names(op))
  if (any(toset)) options(op.dplyr[toset])

  invisible()
}


#' @export
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Thank you for using. This package is still in development stage.")
}
