#' Replace zeros with NAs
#'
#' This function replaces all zeros in a numeric vector with NA values.
#' Useful for sensor data where zero means missing.
#'
#' @param x A numeric vector
#' @return A numeric vector with zeros replaced by NAs
#' @examples
#' replace_zero_na(c(0, 1, 2, 0, 3))
#'
#' @export
replace_zero_na <- function(x) {
  # Functionality: replace 0 with NA
  x[x == 0] <- NA
  return(x)
}
