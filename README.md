This repository contains scripts for computing an Exponential Smoothing base forecast for 
products based on weekly sales data. A seasonal factor scales the base forecast for clusters of product types.
The seasonal clusters are determined based on hierarchical clustering.

### Updating Forecast 
run_forecast.sh executes the script run/update_forecast.R. 
It updates SQL database tables FCST and FCST_INPUT_HISTORY.
Input data are collected from the view V_FCST_INPUT.

A file config.R defines the database credentials. It has to be modified by the user. Alternatively the credentials can 
be add as environment variables on the instance in etc/environment. 

### Updating Cluster Seasonality 
If seasonality components should be updated, the script run/update_seasonality.R has to be executed. 
It automatically updates the table FCST_SEASONALITY and FCST_SEASONALITY_MAPPING. 
Computation input data are collected from the view V_FCST_SEASONALITY_INPUT.

### Installation Requirements
Currently R4.0, the packages devtools, data.table, forecast, imputeTS, TSclust, odbc, DBI, plyr and tsoutliers 
are required for the forecast computation.



