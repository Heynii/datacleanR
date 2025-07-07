#' Standardize numeric vector
#'
#' @param x Numeric vector
#' @return Standardized numeric vector (mean 0, sd 1)
#' @examples
#' standardize(c(1,2,3,4,5))
#' @export
standardize <- function(x) {
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}
