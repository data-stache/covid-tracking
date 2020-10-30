library(tidyverse)
library(lubridate)
library(zoo)

# MOBILITY DATA
url <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
temp_file <- tempfile()
download.file(url, destfile = temp_file)

##### WRANGLE MOBILITY COVID DATA ####
mobility <- read.csv(temp_file)
mobility <- mobility %>%
  filter(country_region_code == "US" & !sub_region_1 == "" & sub_region_2 == "") %>%
  select(sub_region_1, date, retail_and_recreation_percent_change_from_baseline, grocery_and_pharmacy_percent_change_from_baseline,
         parks_percent_change_from_baseline, transit_stations_percent_change_from_baseline, workplaces_percent_change_from_baseline, residential_percent_change_from_baseline) %>%
  rename(state_name = sub_region_1,
         retail_recreation = retail_and_recreation_percent_change_from_baseline,
         grocery_pharmacy = grocery_and_pharmacy_percent_change_from_baseline,
         parks = parks_percent_change_from_baseline,
         transit = transit_stations_percent_change_from_baseline,
         workplace = workplaces_percent_change_from_baseline,
         residential = residential_percent_change_from_baseline) %>%
  mutate(date = ymd(date)) %>%
  group_by(state_name) %>%
  arrange(date) %>%
  mutate(retail_recreation_07da = rollapply(retail_recreation, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         grocery_pharmacy_07da = rollapply(grocery_pharmacy, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         parks_07da = rollapply(parks, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         transit_07da = rollapply(transit, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         workplace_07da = rollapply(workplace, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         residential_07da = rollapply(residential, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"))
file.remove(temp_file)
save(mobility, file = "rda/mobility.rda")

# Last Update
mobility_update <- max(mobility$date)
save(mobility_update, file = "rda/mobility_update.rda")