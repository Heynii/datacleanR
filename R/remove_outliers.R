#' Remove outliers with IQR
#'
#' @param x Numeric vector
#' @return Numeric vector with outliers set to NA
#' @examples
#' remove_outliers(c(1,2,3,100))
#' @export
remove_outliers <- function(x) {
  if (!is.numeric(x)) stop("Input must be numeric.")

  Q1 <- quantile(x,0.25,na.rm=TRUE)
  Q3 <- quantile(x,0.75,na.rm=TRUE)
  IQR <- Q3 - Q1

  lower <- Q1 - 1.5*IQR
  upper <- Q3 + 1.5*IQR

  x[x < lower | x > upper] <- NA
  return(x)
}
