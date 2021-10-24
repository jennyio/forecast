#' mean_wo_outlier
#'
#' @param x vector, numeric
#' @param cut.off.quantile numeric, between 0 and 1
#' @description Computes mean of vector excluding outliers above the cut.off.quantile. 
#' Used for average sales per instock days.
#' @return numeric
#' @export

mean_wo_outlier <- function(x, cut.off.quantile = 0.995) {
  q <- quantile(x, cut.off.quantile)
  m <- mean(x[x < q])
  return(m)
}