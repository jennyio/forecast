#' compute_predictions
#'
#' @param dt.weekly data.table with weekly sales per product. 
#' Contains columns QTY_SCALED, CLUSTER
#' @param seasonality.df data.table, seasonal time series per cluster
#' Contains columns CLUSTER, BEGIN_OF_WEEK, SEASONALITY 
#' @param fcst.weeks date vector, beginning of forecast weeks. 
#' Length corresponds to forecast horizon.
#' @param begin.curr.week character, format YYYY-mm-dd, 
#' Monday of current week to start forecast for.
#'
#' @return data.table with weekly sales forecast for each week in fcst.weeks
#' Contains columns BEGIN_OF_WEEK, FCST_COMPUTATION_DATE, FCST_QTY, BASE_FCST_QTY 
#' 
#' @export

compute_predictions <- function(dt.weekly, seasonality.df,
                                fcst.weeks,
                                begin.curr.week) {
  
  # Apply ets without trend component
  pred.frame <- dt.weekly[, .(CLUSTER = max(CLUSTER),
                              BEGIN_OF_WEEK = as.character(fcst.weeks), 
                              BASE_FCST_QTY = as.vector(forecast(ets(QTY_SCALED, model = 'ZNN'),
                                                                 h = pred.len)$mean)), 
                          by = PRODUCT_ID]
  
  # Join forecasting seasonality
  pred.frame <- merge(pred.frame, 
                      seasonality.df[, c('CLUSTER', 'BEGIN_OF_WEEK', 'SEASONALITY')], 
                      by = c('CLUSTER', 'BEGIN_OF_WEEK'),
                      all.x = T, all.y = F)
  
  # Rescale base quantity to normal level 
  pred.frame[BASE_FCST_QTY < 0.005, BASE_FCST_QTY := 0]
  pred.frame[, FCST_QTY := BASE_FCST_QTY * SEASONALITY]
  pred.frame[, FCST_COMPUTATION_DATE := as.Date(begin.curr.week)]
  
  return(pred.frame)
}
