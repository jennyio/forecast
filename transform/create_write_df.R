
#' create_write_df
#'
#' @param result.list list, per cluster two objects for
#' seasonality forecast (fcst) and input data (target)
#' @param n.years int, n.years * n.weeks = forecast horizon
#' @param n.weeks  int, n.years * n.weeks = forecast horizon
#'
#' @return data.table with columns BEGIN_OF_WEEK, SEASONALITY, CLUSTER
#' to be written to DB
#' @export
#'

create_write_df <- function(result.list, n.years = 2, n.weeks = 52) {
  
  # Initialise seasonality data table
  write.df <- data.table(BEGIN_OF_WEEK = character(), 
                         SEASONALITY = numeric(), 
                         CLUSTER = character())
  write.df[, BEGIN_OF_WEEK := as.Date(BEGIN_OF_WEEK)]
  
  # Iterate through clusters
  # par(mfrow=c(2,2))
  for (g in names(result.list)) {
    f <- result.list[[g]][['fcst']]
    
    # Fitted and forecasted sesonality (trend + seasonal)
    p <- c(f$fitted, f$mean)
    
    # Actual data 
    sdf <- result.list[[g]][['target']]
    
    # Seasonality will be transformed back due to using log average target
    w.df <- data.table(BEGIN_OF_WEEK = c(sdf$BEGIN_OF_WEEK, 
                                         seq(from = max(sdf$BEGIN_OF_WEEK) + 7, 
                                             by = 'week', length.out = n.years*n.weeks)),
                       SEASONALITY = ifelse(exp(p) - 1 < 0, 0, exp(p) - 1))
    w.df[, CLUSTER := g]
    write.df <- rbind(write.df, w.df)
  }
  return(write.df)
}
