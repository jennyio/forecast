
#' compute_corrections
#'
#' @param dt data.table, daily sales data per product with columns 
#' PRODUCT_ID, BEGIN_OF_WEEK, CALENDAR_DATE, UNAVAILABLE, NET_QTY,  
#' CLUSTER, WEEK_SEASONALITY
#' @description De-seasonalising daily data, 
#' perform generic outlier correction, imputation of missing values
#' @return data.table with additional columns MISSING_QTY, OUTLIER_QTY,
#' QTY_CORRECTED, 
#' @export

compute_corrections <- function(dt) {
  
  # Correct qty outliers
  dt[, QTY_SCALED := NET_QTY / WEEK_SEASONALITY]
    # Moving average of scaled qty
  dt[, MA := ma_replace(QTY_SCALED), by = PRODUCT_ID]
  
  # Outlier threshold computation 
  dt[, RESIDUAL := QTY_SCALED - MA]
  dt[, THD := outlier_thd(RESIDUAL), by = PRODUCT_ID]
  
  # Replace outliers by moving average value
  dt[, QTY_SCALED_OUTL_CORR := QTY_SCALED]
  dt[RESIDUAL > THD & is.finite(MA) & NET_QTY > 3, 
     QTY_SCALED_OUTL_CORR := MA]
  
   # Missing value imputation
  dt[, NA_INPUT := QTY_SCALED_OUTL_CORR]

  # Days with no in stock availability will be corrected
  dt[UNAVAILABLE == 0, NA_INPUT := NA]  
  
  # Impute outliers
  dt[, QTY_SCALED_OUTL_MISS_CORR := oos_apply(NA_INPUT),
     by = PRODUCT_ID]
  
  # Correction quantities
  dt[, QTY_CORRECTED := QTY_SCALED_OUTL_MISS_CORR * WEEK_SEASONALITY]
  dt[, MISSING_QTY := (QTY_SCALED_OUTL_CORR - QTY_SCALED_OUTL_MISS_CORR) * WEEK_SEASONALITY]
  dt[, OUTLIER_QTY := (QTY_SCALED - QTY_SCALED_OUTL_CORR) * WEEK_SEASONALITY]

  return(dt[, .(PRODUCT_ID, 
                BEGIN_OF_WEEK,
                SEASONALITY,
                CLUSTER, 
                NET_QTY,
                QTY_CORRECTED, 
                MISSING_QTY, 
                OUTLIER_QTY)])
}