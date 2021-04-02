# CLEAR ENVIRONS
rm(list=ls())

# COVID DATA
url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
dest_file <- "data/covid-confirmed-cases-johns-hopkins.csv"
download.file(url, destfile = dest_file)

url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
dest_file <- "data/covid-confirmed-deaths-johns-hopkins.csv"
download.file(url, destfile = dest_file)

