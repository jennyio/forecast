#!/usr/bin/Rscript

# Required packages
library.path <- .libPaths()
library(data.table, lib.loc = library.path)
library(forecast, lib.loc = library.path)
library(imputeTS, lib.loc = library.path)
library(TSclust, lib.loc = library.path)
library(odbc, lib.loc = library.path)
library(DBI, lib.loc = library.path)
library(plyr, lib.loc = library.path)

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

# Update clusters
DO.RECLUSTER <- FALSE

# Desired length of seasonality forecast
n.years <- 2
n.weeks <- 52

# Read seasonality computation functions
source('src/compute_total_seasonality.R')
source('src/compute_cluster_seasonality.R')
source('src/fcst_utils.R')

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

# Monday of current week
begin.curr.week <- cut(start.time, breaks = 'week', 
                       start.on.monday = TRUE)

# Base data for seasonality computation
query <- paste0("SELECT * FROM DBO.V_FCST_SEASONALITY_INPUT
                 WHERE BEGIN_OF_WEEK < '", begin.curr.week, "'")
# Read input data
df <- get_dt(C, query)
df[, BEGIN_OF_WEEK := as.Date(BEGIN_OF_WEEK)]

end.time <- Sys.time()
time.taken <- end.time - start.time
print(paste0('Query Time: ', round(time.taken, 3)))
odbc::odbcClose(C)

###----------------------------------------------------------------------------
### Define Clusters and Compute Seasonal Decomposition
###----------------------------------------------------------------------------


start.time <- Sys.time()

# Create input clusters based on category level 1 and 2
df <- create_cat_lvl(df, 'CATEGORY_1', 'CATEGORY_2')

# Update clusters of category levels
seas.groups <- cluster_category_levels(df, DO.RECLUSTER=TRUE, 
                                       cat.lvl = 'CATEGORY_LEVEL')

# Decompose log average sales per product in each cluster
result.list <- compute_seasonal_decomposition(df, seas.groups, 
                                              cat.lvl = 'CATEGORY_LEVEL',
                                              target.var = 'LOG_SALES_NORM', 
                                              n.years = n.years,
                                              n.weeks = n.weeks)

# Create table to be written to DB
write.df <- create_write_df(result.list)

###----------------------------------------------------------------------------
### Write Seasonal Forecast / Cluster Mapping to Database
###----------------------------------------------------------------------------

# Connect to database
C <- DBI::dbConnect(odbc::odbc(),
                    Driver = "SQL Server",
                    Server = server.name,
                    Database = db.name,
                    uid = uid,
                    pwd = pwd,
                    options(connectionObserver = NULL))

print('Start saving seasonality decompsition results.')

# Write seasonality per cluster to DB 
write_table(C, write.df,  
            table = "DBO.FCST_SEASONALITY", 
            cols = c("BEGIN_OF_WEEK", "SEASONALITY", "CLUSTER"), 
            types = c("DATE", "DECIMAL(13, 5)", "VARCHAR(30)"))

if (DO.RECLUSTER) {
    # Write mapping from category level to cluster to DB
    write_cluster_mapping(C, seas.groups, 
                          table = 'DBO.FCST_SEASONALITY_MAPPING', 
                          cols = c('CLUSTER', 'CATEGORY_LEVEL'), 
                          types = c("VARCHAR(30)", "VARCHAR(50)"))
}

odbc::odbcClose(C)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(paste0('Cluster Season Computation Time: ', round(time.taken, 3)))
