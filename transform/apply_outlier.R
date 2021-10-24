
applyTso <- function(df, target.var, ts.start,
                     rng.exclude = c("11-15", "01-15"), 
                     verbose = F) {
  
  ### Returns df with additional corrected target time series
  ### and matrix with external regressor variables used for correction
  
  # create time series
  ts.init <- sales.df[, get(target.var)]
  
  ts.init <- msts(ts.init, start = ts.start,
                       seasonal.periods = c(52.18))
  # remove outliers 
  # delta = 0.3 neccecary to decrease the length of TC outliers
  detect <- tso(ts.init, types = c("SLS", "AO", "TC"), delta = 0.5)
  
  if (nrow(detect$outliers) == 0) {
    print('No Outliers Detected')
    tso.correction <- df[, get(target.var)]
    return(tso.correction)
  }
  
  if (verbose) {
    plot(detect)  
  }
  # exclude December outliers
  # remove detected outliers
  
  correction <- as.vector(detect$effects)
  
  df <- copy(sales.df)
  # Negative outlier values should not be corrected.
  df[, correction := correction]
  df[correction < 0, correction := 0]
  df[strftime(BEGIN_OF_WEEK, "%m-%d") >= rng.exclude[1] | 
     strftime(BEGIN_OF_WEEK, "%m-%d") <= rng.exclude[2], correction := 0]
  
  tso.correction <- df[, get(target.var) - correction]
 
  return(tso.correction)
} 