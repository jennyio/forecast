#' oos_apply
#'
#' @param x_input numeric, 
#' @description Imputes daily sales based on weekday average
#' If ts is too short, mean will be used for imputation.
#' Requires the data to contain column NA_INPUT
#' Try weekday replacement first. 
#' If there are no data for a specific weekday, overall mean will be used.
#' If there are less than two weeks of data available, overall mean will be used.
#' For the last two cases for each weekday, check if scaled sales are above average.
#' @return numeric vector 
#' @export

oos_apply <- function(x_input) {
  
  
  # Implemented to decrease forecast for new items which run out of stock quickly.
  n.weekd <- 7
  x <- x_input
  
  # If there are no missing data, continue
  if (!anyNA(x)) {
    return(x)
  }
  FEW.DATA.CHECK <- FALSE
  
  # Try NA correction on products which have at least 14 days of history
  if (length(x) > 14) {
    for (i in 1:n.weekd) {
      indices <- seq(from = i, to = length(x), by = n.weekd)
      x.tmp <- x[indices]
      
      
      if (!anyNA(x.tmp)) {
        next
      }
      
      miss.tmp <- is.na(x.tmp)
      # For less than two weekdays, overall average will be used.
      if (sum(!miss.tmp) < 2) {
        next
      }
      else {
        x.mean <- mean(x.tmp, na.rm = TRUE)
        x.tmp[miss.tmp] <- x.mean
        x[indices] <- x.tmp
      }
    } 
    # If some weekdays have no sales, 
    # replace by overall mean.
    if(anyNA(x)) {
      miss.x <- is.na(x)
      x.mean <- mean(x, na.rm = TRUE)
      x[miss.x] <- x.mean
      FEW.DATA.CHECK <- TRUE
    }
  } else {
    miss.x <- is.na(x)
    x.mean <- mean(x, na.rm = TRUE)
    x[miss.x] <- x.mean
    FEW.DATA.CHECK <- TRUE
  }
  
  if(FEW.DATA.CHECK) {
    # NA corrected value > mean target value, it will be replaced
    repl.ind <- (as.vector(x > x.mean) & miss.x)
    x[repl.ind] <- x.mean
  }
  return(x)
}