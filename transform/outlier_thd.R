#' outlier_thd
#'
#' @param x 
#' @description Compute cut off threshold for time series outliers based on 
#' 75% quantile + 1.5 * inter quartile range
#' @return numeric
#' @export

outlier_thd <- function(x) {
  # Function computes upper threshold for outliers.
  thd <- quantile(x, 0.75, na.rm = T) + 1.5 * IQR(x, na.rm = T)
  return(thd)
}