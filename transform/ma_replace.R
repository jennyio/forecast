
#' ma_replace
#'
#' @param x numeric, vector containing  values
#'
#' @return 
#' @export

ma_replace <- function(x) {
  if(sum(is.na(x)) > 0) {
    stop("Daily time series contains NA values")
  }
  # Computes 7 days moving average and replaces NA values
  if (length(x) < 7) {
    return(mean(x))
  } else {
    # Pad beginning and end with last/first MA value
    m <- as.vector(forecast::ma(x, order = 7))
    return(imputeTS::na_locf(m))
  }
}