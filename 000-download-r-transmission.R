# CLEAR ENVIRONS
rm(list=ls())

# COVID DATA
url <- "https://d14wlfuexuxgcm.cloudfront.net/covid/rt.csv"
dest_file <- "data/r-transmission.csv"
download.file(url, destfile = dest_file)


