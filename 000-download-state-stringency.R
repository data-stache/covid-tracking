# CLEAR ENVIRONS
rm(list=ls())

# COVID DATA
url <- "https://raw.githubusercontent.com/OxCGRT/USA-covid-policy/master/data/OxCGRT_US_latest.csv"
dest_file <- "data/usa-covid-policy-by-state.csv"
download.file(url, destfile = dest_file)

