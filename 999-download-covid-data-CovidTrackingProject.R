# CLEAR ENVIRONS
rm(list=ls())

# COVID DATA
url <- "https://api.covidtracking.com/v1/states/daily.csv"
dest_file <- "data/covid-daily-report-by-state.csv"
download.file(url, destfile = dest_file)
