#' Convert centimeters to inches
#'
#' This function converts a numeric value from centimeters to inches.
#'
#' @param x A numeric value representing the length in centimeters.
#'
#' @return A numeric value representing the length in inches.
#'
#' @examples
#' cm2inche(10)
#' cm2inche(25.4)
#'
#' @export
cm2inche <- function(x) {
  stopifnot(is.numeric(x))
  return(x / 2.54)
}
