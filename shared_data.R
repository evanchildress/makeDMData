library(RPostgreSQL)
library(lubridate)
library(integrator)
library(parallel)
library(reshape2)
library(ggplot2)
#library(cruftery)

options(stringsAsFactors=FALSE)
options(check.names=FALSE)

shared_data <<- local(expr={
	link <- db_connector(credentials="~/wb_credentials.rds")
	data_root <- "~/process-data/data_store/processed_data"
	## This is where the global env gets a 'shared_data' as constructed above.
	return(environment(NULL))   
})




