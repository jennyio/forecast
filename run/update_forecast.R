#!/usr/bin/Rscript

library.path <- .libPaths()
library(data.table, lib.loc = library.path)
library(forecast, lib.loc = library.path)
library(imputeTS, lib.loc = library.path)
library(odbc, lib.loc = library.path)
library(DBI, lib.loc = library.path)

# Set working directory
setwd("/app")

# Source functions
file.sources <- list.files(c("model/", "transform/", "utils/"), 
                           pattern="*.R$", full.names=TRUE, ignore.case=TRUE)

sapply(file.sources, source)
###----------------------------------------------------------------------------
### Configurations
###----------------------------------------------------------------------------

# Get credentials for Exasol from environment variables
env.vars <- Sys.getenv(c('HOST', 'PORT', 'USER', 'PWD'))

if (any(env.vars=="")) {
  # DB credentials should be defined in the config file or as environment variables.
  source('config.R')  
} else {
  host <- env.vars[['HOST']]
  port <- env.vars[['PORT']]
  uid  <- env.vars[['USER']]
  pwd  <- env.vars[['PWD']]
}

server.name <- "ServerName"
db.name     <- "DatabaseName"
db.ip       <- paste(host, port, sep=':')

# Connect to database
C <- DBI::dbConnect(odbc::odbc(),
                    Driver = "SQL Server",
                    Server = server.name,
                    Database = db.name,
                    uid = uid,
                    pwd = pwd,
                    options(connectionObserver = NULL))

###----------------------------------------------------------------------------
## Fetch data from database 
###----------------------------------------------------------------------------


start.time <- Sys.time()
begin.curr.week <- cut(start.time, breaks = 'week', 
                       start.on.monday = TRUE)

pred.len    <- 52
fcst.weeks  <- seq(from = as.Date(begin.curr.week), 
                  length.out = pred.len, by = 'week')

# Create input query
input.view  <- "DBO.V_FCST_INPUT"
query       <- input_query(begin.curr.week, input.view)

# Read forecast input data
df <- get_dt(C, query)

# Use a weekly seasonality for category clusters to scale the output 
# of the baseline exponential smoothing forecast

# Read seasonality data per cluster for forecasting
seas.df     <- get_dt(C, "SELECT * FROM DBO.FCST_SEASONALITY")
# Read mapping from category level to seasonality cluster
seas.map.df <- get_dt(C, "SELECT * FROM DBO.FCST_SEASONALITY_MAPPING")

odbc::odbcClose(C)

end.time    <- Sys.time()
time.taken  <- end.time - start.time
print(paste0('Read Data Time: ', round(time.taken, 3)))

###----------------------------------------------------------------------------
### Preprocess Data for Forecast Computation 
###----------------------------------------------------------------------------


#sapply(df, FUN = function(x) sum(is.na(x)))
start.time <- Sys.time()

# Create column CATEGORY_LEVEL
df <- create_cat_lvl(df, 'CATEGORY_1', 'CATEGORY_2')

# Join seasonality to sales data. Use CLUSTER := .. to avoid inner join (left join intended)
df <- df[seas.map.df[, .(CATEGORY_LEVEL, CLUSTER)], 
         on = 'CATEGORY_LEVEL',  
         CLUSTER := i.CLUSTER]

df <- merge(df, seas.df[, .(CLUSTER, BEGIN_OF_WEEK, SEASONALITY)], 
            on = c('CLUSTER', 'BEGIN_OF_WEEK'), 
            all.x = TRUE, all.y = FALSE)

df <- df[order(PRODUCT_ID, CALENDAR_DATE), ]

if (length(unique(df[, CLUSTER])) < (length(unique(seas.map.df[, CLUSTER])) - 2)) {
  print('Warning: Product category levels do not match seasonality clusters')
}

# Add 1 to not divide target quantity by too small numbers
df[, WEEK_SEASONALITY := (SEASONALITY + 1) * WEEKLY_NET_QTY_DISTRIBUTION]

###----------------------------------------------------------------------------
### Correct outliers and missing data
###----------------------------------------------------------------------------


df <- compute_corrections(df)

###----------------------------------------------------------------------------
### Weekly aggregation
###----------------------------------------------------------------------------


df.weekly  <- compute_weekly_data(df, begin.curr.week)

end.time   <- Sys.time()
time.taken <- end.time - start.time
print(paste0('Data Transformation Time: ', round(time.taken, 3)))

###----------------------------------------------------------------------------
### Model Prediction
###----------------------------------------------------------------------------


start.time <- Sys.time()

# Apply ets without trend component
pred.frame <- compute_predictions(df.weekly, seas.df,
                                  fcst.weeks, begin.curr.week)

end.time   <- Sys.time()
time.taken <- end.time - start.time
print(paste0('Model Computation Time: ', round(time.taken, 3)))

###----------------------------------------------------------------------------
### Write Data
###----------------------------------------------------------------------------


C <- DBI::dbConnect(odbc::odbc(),
                    Driver = "SQL Server",
                    Server = server.name,
                    Database = db.name,
                    uid = uid,
                    pwd = pwd,
                    options(connectionObserver = NULL))
					
# Input data used for forecast computations
# Table will be overwritten
input.hist.table <- 'DBO.FCST_INPUT_HISTORY'
input.hist.cols  <- c('PRODUCT_ID', 'BEGIN_OF_WEEK', 'INPUT_QTY',
                      'SEASONALITY', 'MISSING_QTY', 'OUTLIER_QTY',
                      'NET_QTY', 'CLUSTER', 'FCST_COMPUTATION_DATE')
input.hist.types <- c('VARCHAR(40) UTF8', 'DATE', 'DECIMAL(13,5)',
                      'DECIMAL(13,5)', 'DECIMAL(13,5)', 'DECIMAL(13,5)', 
                      'DECIMAL(13,5)', 
                      'VARCHAR(40) UTF8', 'DATE')
write_table(C, input.hist.table, input.hist.cols, input.hist.types,
            overwrite = TRUE)

# Write current weekly forecast to FCST
fcst.hist.table <- 'DBO.FCST'
fcst.hist.cols  <- c('CLUSTER', 'BEGIN_OF_WEEK', 'PRODUCT_ID',
                     'BASE_FCST_QTY', 'SEASONALITY', 'FCST_QTY',
                     'FCST_COMPUTATION_DATE')
fcst.hist.types <- c('VARCHAR(50) UTF8', 'DATE', 'VARCHAR(50) UTF8', 
                     'DECIMAL(13,5)', 'DECIMAL(13,5)', 'DECIMAL(13,5)', 
                     'DATE')

create_table(C, fcst.hist.table, fcst.hist.cols, fcst.hist.types)
# Delete current weekly forecast to FCST_HISTORY 
DBI::dbExecute(C, paste0("DELETE FROM ", fcst.hist.table, 
                         " WHERE FCST_COMPUTATION_DATE = '",
                         begin.curr.week, "'"))
# Write new data
odbc::dbWriteTable(C, 
                   name = fcst.hist.table,
                   value = pred.frame[, fcst.hist.cols, with = FALSE])

n.row.fcst.hist <- DBI::dbExecute(C, paste0("SELECT COUNT(*) FROM ", 
                                            fcst.hist.table,
                                            " WHERE FCST_COMPUTATION_DATE = '",
                                            begin.curr.week, "'"))
print(paste0('FCST History rows: ', n.row.fcst.hist))

odbc::odbcClose(C)

print("Finished Forecast Computation")