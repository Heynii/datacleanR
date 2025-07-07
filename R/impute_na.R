#' Impute missing values
#'
#' @param x Numeric vector
#' @param method Method: "mean", "median" or numeric value
#' @return Numeric vector with imputed values
#' @examples
#' impute_na(c(1,NA,3), method="mean")
#' impute_na(c(1,NA,3), method=0)
#' @export
impute_na <- function(x, method="mean") {
  if(method=="mean") {
    val <- mean(x, na.rm=TRUE)
  } else if(method=="median") {
    val <- median(x, na.rm=TRUE)
  } else if(is.numeric(method)) {
    val <- method
  } else {
    stop("Invalid method")
  }
  x[is.na(x)] <- val
  return(x)
}
