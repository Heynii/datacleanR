#' Standardize numeric vector
#'
#' @param x Numeric vector
#' @return Standardized numeric vector (mean 0, sd 1)
#' @examples
#' standardize(c(1,2,3,4,5))
#' @export
standardize <- function(x) {
  if (!is.numeric(x)) stop("Input must be numeric.")

  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)

  if (s == 0) return(rep(0, length(x)))

  return((x - m) / s)
}
