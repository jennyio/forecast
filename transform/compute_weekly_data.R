
#' compute_weekly_data
#'
#' @param dt data.table for daily sales including outlier,
#' missing value corrections and cluster seasonality. 
#' Containing columns PRODUCT_ID, BEGIN_OF_WEEK, SEASONALITY, CLUSTER, 
#' QTY_CORRECTED, MISSING_QTY, OUTLIER_QTY, NET_QTY
#' @param begin.curr.week character, format YYYY-mm-dd
#'
#' @return data.table with weekly forecast quantities. 
#' Containing columns INPUT_QTY, SEASONALITY, CLUSTER,  
#' NET_QTY, OUTLIER_QTY, MISSING_QTY, QTY_SCALED, 
#' PRODUCT_ID, BEGIN_OF_WEEK, FCST_COMPUTATION_DATE
#' @export

compute_weekly_data <- function(dt, begin.curr.week) {
  # Aggregate daily data to weekly level 
  dt.weekly <- dt[BEGIN_OF_WEEK < as.character(begin.curr.week), 
                  list(INPUT_QTY = round(sum(QTY_CORRECTED), 3), 
                       SEASONALITY = mean(SEASONALITY),
                       MISSING_QTY = round(sum(MISSING_QTY), 3),
                       OUTLIER_QTY = round(sum(OUTLIER_QTY), 3),
                       NET_QTY = round(sum(NET_QTY), 3),
                       CLUSTER = max(CLUSTER)), 
                  by = .(PRODUCT_ID, BEGIN_OF_WEEK)]
  
  dt.weekly[, QTY_SCALED := round(INPUT_QTY / SEASONALITY, 3)]
  dt.weekly[!is.finite(QTY_SCALED), QTY_SCALED := 0]
  # Computation date is set to beginning from current week
  dt.weekly[, FCST_COMPUTATION_DATE := as.Date(begin.curr.week)]
  return(dt.weekly)
}