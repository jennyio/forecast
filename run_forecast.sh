
#!/bin/bash

LOGFILE=/home/ubuntu/app/log/log_forecast_$(date +"%Y-%m-%d_%H%M").txt

exec 3>&1 1>$LOGFILE

Rscript /home/ubuntu/app/run/update_forecast.R --verbose

exec 1>&3 3>&-
