#' compute_seasonal_decomposition
#'
#' @param df data.table, one row per week, category_1/category_2. 
#' Contains columns QTY, NUM_INSTOCK_ITEMS, BEGIN_OF_WEEK
#' @param seas.groups list, mapping of category levels to clusters.
#' Names are cluster names and corresponding list of values of cat.lvl 
#' @param cat.lvl character, column name of categorical variable 
#' @param target.var character, variable used for seasonal decomposition.
#' Possible are LOG_TARGET_NORM, LOG_TARGET, TARGET_NORM, TARGET
#' @param n.years int, number if years for seasonality forecast
#' @param n.weeks int, number if years for seasonality forecast
#' @description Compute seasonal decomposition of target variable per 
#' cluster.
#' @return list, per cluster the seasonal and trend forecast of length
#' n.years * n.weeks
#' 
#' @export

compute_seasonal_decomposition <- function(df, seas.groups, 
                                           cat.lvl = 'CATEGORY_LEVEL',
                                           target.var = 'LOG_TARGET_NORM', 
                                           n.years = 2, n.weeks = 52) {
  
  result.list <- list()
  # Iterate over clusters
  for (g in names(seas.groups)) { 
    print(g)
    
    # All available dates
    date.rng <- range(df[, BEGIN_OF_WEEK])
    date.seq <- seq(from = date.rng[1], to = date.rng[2], by = 'week')
    date.df <- data.table(BEGIN_OF_WEEK = date.seq)
    
    # Use data belonging to cluster and group by week
    target.df <- df[get(cat.lvl) %in% seas.groups[g][[1]], 
                    list(TARGET = sum(QTY),
                         TARGET_NORM = sum(QTY) / sum(NUM_INSTOCK_ITEMS))
                    , by = BEGIN_OF_WEEK]
    first.date <- min(target.df[, BEGIN_OF_WEEK])
    
    # Add days with missing stock dates
    target.df <- merge(target.df, date.df, by = 'BEGIN_OF_WEEK', all = TRUE)
    target.df[is.na(TARGET), TARGET := 0]
    target.df[is.na(TARGET), TARGET_NORM := 0]
    
    target.df <- target.df[BEGIN_OF_WEEK >= first.date, ]
    
    # Get starting date of time series in vector format 
    ts.start <- as.numeric(c(substr(first.date, 1, 4), 
                             substr(first.date, 6, 7), 
                             substr(first.date, 9, 10)))
    
    # Compute logs of target
    cols <- c('TARGET', 'TARGET_NORM')
    target.df[, paste('LOG', cols, sep = '_') := lapply(.SD, 
                                                        function(x) ifelse(!is.na(x), log(x + 1), 0)), 
              .SDcols = cols]
    
    target.df <- target.df[order(BEGIN_OF_WEEK), ]
    target.df[, BEGIN_OF_WEEK := as.Date(BEGIN_OF_WEEK)]
    
    # Convert to time series type
    ts <- target.df[, get(target.var)]
    ts <- msts(ts, start = ts.start, seasonal.periods = c(52.18))
    
    # Decompose target variable
    stl_ts <- stl(ts, s.window = 'periodic', robust = FALSE)
    
    # Add trend and seasonal components of the decomposition to data
    target.df[, TREND := as.vector(stl_ts$time.series[,'trend'])]
    target.df[, SEASONAL := as.vector(stl_ts$time.series[,'seasonal'])]
    
    # Forecast seasonality
    fc <- forecast(stl_ts, h = n.years*n.weeks)
    
    result.list[[g]] <- list('fcst' = fc, 'target' = target.df)
  }
  return(result.list)
}