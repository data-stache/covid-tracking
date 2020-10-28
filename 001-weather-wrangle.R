library(broom)
library(rnoaa)
options(noaakey = "kGRXilHWEaJJxeggfEjQZbeOKrhsIGDg")
library(stringr)
library(tidyverse)
library(zoo)
library(lubridate)

load("rda/covid.rda")

##### 01 AL ALABAMA #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:01", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
al_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_al <- meteo_pull_monitors(al_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_al <- weather_al %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL CT STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "AL") %>%
  ungroup()
# REORDER COLUMNS
weather_al <- weather_al[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- weather_al

##### 02 AK ALASKA #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:02", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
ak_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_ak <- meteo_pull_monitors(ak_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_ak <- weather_ak %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL CT STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "AK") %>%
  ungroup()
# REORDER COLUMNS
weather_ak <- weather_ak[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_ak)


##### 04 AZ ARIZONA #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:04", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
az_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_az <- meteo_pull_monitors(az_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_az <- weather_az %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL CT STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "AZ") %>%
  ungroup()
# REORDER COLUMNS
weather_az <- weather_az[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_az)


##### 05 AR ARKANSAS #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:05", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
ar_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_ar <- meteo_pull_monitors(ar_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_ar <- weather_ar %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL CT STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "AR") %>%
  ungroup()
# REORDER COLUMNS
weather_ar <- weather_ar[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_ar)


##### 06 CA CALIFORNIA #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:06", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
CA_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_CA <- meteo_pull_monitors(CA_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_CA <- weather_CA %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL CT STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "CA") %>%
  ungroup()
# REORDER COLUMNS
weather_CA <- weather_CA[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_CA)


##### 08 C0 COLORADO #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:08", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
CO_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_CO <- meteo_pull_monitors(CO_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_CO <- weather_CO %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL CT STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "CO") %>%
  ungroup()
# REORDER COLUMNS
weather_CO <- weather_CO[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_CO)


##### 09 CT CONNECTICUT #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:09", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
CT_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_CT <- meteo_pull_monitors(CT_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_CT <- weather_CT %>%
  arrange(date) %>%
  # CTNVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL CT STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "CT") %>%
  ungroup()
# REORDER CTLUMNS
weather_CT <- weather_CT[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_CT)


##### 10 DE DELAWARE #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:10", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
DE_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_DE <- meteo_pull_monitors(DE_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_DE <- weather_DE %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL DE STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "DE") %>%
  ungroup()
# REORDER COLUMNS
weather_DE <- weather_DE[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_DE)


##### 12 FL FLORIDA #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:12", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
FL_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_FL <- meteo_pull_monitors(FL_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_FL <- weather_FL %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL FL STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "FL") %>%
  ungroup()
# REORDER COLUMNS
weather_FL <- weather_FL[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_FL)


##### 13 GA GEORGIA #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:13", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
GA_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_GA <- meteo_pull_monitors(GA_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_GA <- weather_GA %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL GA STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "GA") %>%
  ungroup()
# REORDER COLUMNS
weather_GA <- weather_GA[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_GA)


##### 15 HI HAWAII #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:15", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
HI_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_HI <- meteo_pull_monitors(HI_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_HI <- weather_HI %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL HI STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "HI") %>%
  ungroup()
# REORDER COLUMNS
weather_HI <- weather_HI[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_HI)


##### 16 ID IDAHO #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:16", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
ID_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_ID <- meteo_pull_monitors(ID_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_ID <- weather_ID %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL ID STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "ID") %>%
  ungroup()
# REORDER COLUMNS
weather_ID <- weather_ID[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_ID)


##### 17 IL ILLINOIS #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:17", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
IL_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_IL <- meteo_pull_monitors(IL_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_IL <- weather_IL %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL IL STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "IL") %>%
  ungroup()
# REORDER COLUMNS
weather_IL <- weather_IL[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_IL)


##### 18 IN INDIANA #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:18", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
IN_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_IN <- meteo_pull_monitors(IN_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_IN <- weather_IN %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL IN STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "IN") %>%
  ungroup()
# REORDER COLUMNS
weather_IN <- weather_IN[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_IN)


##### 19 IA IOWA #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:19", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
IA_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_IA <- meteo_pull_monitors(IA_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_IA <- weather_IA %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL IA STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "IA") %>%
  ungroup()
# REORDER COLUMNS
weather_IA <- weather_IA[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_IA)


##### 20 KS KANSAS #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:20", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
KS_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_KS <- meteo_pull_monitors(KS_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_KS <- weather_KS %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL KS STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "KS") %>%
  ungroup()
# REORDER COLUMNS
weather_KS <- weather_KS[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_KS)


##### 21 KY KENTUCKY #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:21", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
KY_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_KY <- meteo_pull_monitors(KY_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_KY <- weather_KY %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL KY STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "KY") %>%
  ungroup()
# REORDER COLUMNS
weather_KY <- weather_KY[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_KY)


##### 22 LA LOUISIANA #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:22", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
LA_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_LA <- meteo_pull_monitors(LA_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_LA <- weather_LA %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL LA STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "LA") %>%
  ungroup()
# REORDER COLUMNS
weather_LA <- weather_LA[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_LA)


##### 23 ME MAINE #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:23", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
ME_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_ME <- meteo_pull_monitors(ME_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_ME <- weather_ME %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL ME STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "ME") %>%
  ungroup()
# REORDER COLUMNS
weather_ME <- weather_ME[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_ME)


##### 24 MD MARYLAND #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:24", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
MD_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_MD <- meteo_pull_monitors(MD_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_MD <- weather_MD %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL MD STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "MD") %>%
  ungroup()
# REORDER COLUMNS
weather_MD <- weather_MD[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_MD)


##### 25 MA MASSACHUSSETTS #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:25", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
MA_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_MA <- meteo_pull_monitors(MA_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_MA <- weather_MA %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL MA STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "MA") %>%
  ungroup()
# REORDER COLUMNS
weather_MA <- weather_MA[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_MA)


##### 26 MI MICHIGAN #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:26", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
MI_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_MI <- meteo_pull_monitors(MI_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_MI <- weather_MI %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL MI STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "MI") %>%
  ungroup()
# REORDER COLUMNS
weather_MI <- weather_MI[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_MI)


##### 27 MN MINNESOTA #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:27", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
MN_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_MN <- meteo_pull_monitors(MN_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_MN <- weather_MN %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL MN STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "MN") %>%
  ungroup()
# REORDER COLUMNS
weather_MN <- weather_MN[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_MN)


##### 28 MS MISSISSIPPI #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:28", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
MS_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_MS <- meteo_pull_monitors(MS_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_MS <- weather_MS %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL MS STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "MS") %>%
  ungroup()
# REORDER COLUMNS
weather_MS <- weather_MS[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_MS)


##### 29 MO MISSOURI #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:29", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
MO_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_MO <- meteo_pull_monitors(MO_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_MO <- weather_MO %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL MO STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "MO") %>%
  ungroup()
# REORDER COLUMNS
weather_MO <- weather_MO[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_MO)


##### 30 MT MONTANA #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:30", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
MT_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_MT <- meteo_pull_monitors(MT_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_MT <- weather_MT %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL MT STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "MT") %>%
  ungroup()
# REORDER COLUMNS
weather_MT <- weather_MT[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_MT)


##### 31 NE NEBRASKA #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:31", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
NE_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_NE <- meteo_pull_monitors(NE_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_NE <- weather_NE %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL NE STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "NE") %>%
  ungroup()
# REORDER COLUMNS
weather_NE <- weather_NE[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_NE)


##### 32 NV NEVADA #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:32", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
NV_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_NV <- meteo_pull_monitors(NV_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_NV <- weather_NV %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL NV STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "NV") %>%
  ungroup()
# REORDER COLUMNS
weather_NV <- weather_NV[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_NV)


##### 33 NH NEW HAMPSHIRE #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:33", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
NH_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_NH <- meteo_pull_monitors(NH_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_NH <- weather_NH %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL NH STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "NH") %>%
  ungroup()
# REORDER COLUMNS
weather_NH <- weather_NH[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_NH)


##### 34 NJ NEW JERSEY #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:34", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
NJ_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_NJ <- meteo_pull_monitors(NJ_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_NJ <- weather_NJ %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL NJ STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "NJ") %>%
  ungroup()
# REORDER COLUMNS
weather_NJ <- weather_NJ[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_NJ)


##### 35 NM NEW MEXICO #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:35", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
NM_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_NM <- meteo_pull_monitors(NM_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_NM <- weather_NM %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL NM STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "NM") %>%
  ungroup()
# REORDER COLUMNS
weather_NM <- weather_NM[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_NM)


##### 36 NY NEW YORK #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:36", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
NY_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_NY <- meteo_pull_monitors(NY_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_NY <- weather_NY %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL NY STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "NY") %>%
  ungroup()
# REORDER COLUMNS
weather_NY <- weather_NY[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_NY)


##### 37 NC NORTH CAROLINA #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:37", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
NC_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_NC <- meteo_pull_monitors(NC_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_NC <- weather_NC %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL NC STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "NC") %>%
  ungroup()
# REORDER COLUMNS
weather_NC <- weather_NC[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_NC)


##### 38 ND NORTH DAKOTA #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:38", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
ND_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_ND <- meteo_pull_monitors(ND_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_ND <- weather_ND %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL ND STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "ND") %>%
  ungroup()
# REORDER COLUMNS
weather_ND <- weather_ND[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_ND)


##### 39 OH OHIO #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:39", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
OH_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_OH <- meteo_pull_monitors(OH_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_OH <- weather_OH %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL OH STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "OH") %>%
  ungroup()
# REORDER COLUMNS
weather_OH <- weather_OH[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_OH)


##### 40 OK OKLAHOMA #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:40", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
OK_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_OK <- meteo_pull_monitors(OK_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_OK <- weather_OK %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL OK STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "OK") %>%
  ungroup()
# REORDER COLUMNS
weather_OK <- weather_OK[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_OK)


##### 41 OR OREGON #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:41", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
OR_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_OR <- meteo_pull_monitors(OR_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_OR <- weather_OR %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL OR STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "OR") %>%
  ungroup()
# REORDER COLUMNS
weather_OR <- weather_OR[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_OR)


##### 42 PA PENNSYLVANIA #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:42", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
PA_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_PA <- meteo_pull_monitors(PA_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_PA <- weather_PA %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL PA STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "PA") %>%
  ungroup()
# REORDER COLUMNS
weather_PA <- weather_PA[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_PA)


##### 44 RI RHODE ISLAND #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:44", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
RI_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_RI <- meteo_pull_monitors(RI_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_RI <- weather_RI %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL RI STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "RI") %>%
  ungroup()
# REORDER COLUMNS
weather_RI <- weather_RI[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_RI)


##### 45 SC SOUTH CAROLINA #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:45", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
SC_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_SC <- meteo_pull_monitors(SC_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_SC <- weather_SC %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL SC STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "SC") %>%
  ungroup()
# REORDER COLUMNS
weather_SC <- weather_SC[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_SC)


##### 46 SD SOUTH DAKOTA #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:46", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
SD_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_SD <- meteo_pull_monitors(SD_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_SD <- weather_SD %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL SD STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "SD") %>%
  ungroup()
# REORDER COLUMNS
weather_SD <- weather_SD[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_SD)


##### 47 TN TENNESSEE #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:47", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
TN_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_TN <- meteo_pull_monitors(TN_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_TN <- weather_TN %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL TN STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "TN") %>%
  ungroup()
# REORDER COLUMNS
weather_TN <- weather_TN[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_TN)


##### 48 TX GEORGIA #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:48", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
TX_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_TX <- meteo_pull_monitors(TX_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_TX <- weather_TX %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL TX STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "TX") %>%
  ungroup()
# REORDER COLUMNS
weather_TX <- weather_TX[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_TX)


##### 49 UT UTAH #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:49", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
UT_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_UT <- meteo_pull_monitors(UT_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_UT <- weather_UT %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL UT STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "UT") %>%
  ungroup()
# REORDER COLUMNS
weather_UT <- weather_UT[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_UT)


##### 50 VT VERMONT #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:50", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
VT_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_VT <- meteo_pull_monitors(VT_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_VT <- weather_VT %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL VT STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "VT") %>%
  ungroup()
# REORDER COLUMNS
weather_VT <- weather_VT[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_VT)


##### 51 VA VIRGINIA #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:51", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
VA_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_VA <- meteo_pull_monitors(VA_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_VA <- weather_VA %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL VA STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "VA") %>%
  ungroup()
# REORDER COLUMNS
weather_VA <- weather_VA[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_VA)


##### 53 WA WASHINGTON #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:53", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
WA_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_WA <- meteo_pull_monitors(WA_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_WA <- weather_WA %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL WA STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "WA") %>%
  ungroup()
# REORDER COLUMNS
weather_WA <- weather_WA[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_WA)


##### 54 WV WEST VIRGINIA #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:54", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
WV_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_WV <- meteo_pull_monitors(WV_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_WV <- weather_WV %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL WV STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "WV") %>%
  ungroup()
# REORDER COLUMNS
weather_WV <- weather_WV[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_WV)


##### 55 WI WISCONSIN #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:55", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
WI_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_WI <- meteo_pull_monitors(WI_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_WI <- weather_WI %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL WI STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "WI") %>%
  ungroup()
# REORDER COLUMNS
weather_WI <- weather_WI[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_WI)


##### 56 WY WYOMING #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:56", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
WY_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_WY <- meteo_pull_monitors(WY_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_WY <- weather_WY %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL WY STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "WY") %>%
  ungroup()
# REORDER COLUMNS
weather_WY <- weather_WY[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_WY)


##### 72 PR PUERTO RICO #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:72", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
PR_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_PR <- meteo_pull_monitors(PR_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_PR <- weather_PR %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL PR STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "PR") %>%
  ungroup()
# REORDER COLUMNS
weather_PR <- weather_PR[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_PR)


##### 11 DC WASHINGTON DC #####
stations <- ncdc(datasetid="GHCND", locationid = "FIPS:11", startdate = "2020-03-01", enddate = "2020-03-31", datatypeid = c("TMAX", "TMIN"))
stations <- stations[[2]]
DC_stations <- unique(stations$station) %>%
  str_replace(.,"GHCND:", "")

weather_DC <- meteo_pull_monitors(DC_stations, date_min = "2020-03-01", var = c("TMIN", "TMAX"))

weather_DC <- weather_DC %>%
  arrange(date) %>%
  # CONVERT TO °F
  mutate(tmax = ((tmax * .1) * (9/5)) + 32,
         tmin = ((tmin * .1) * (9/5)) + 32,
         # ADD AVERAGE TEMPERATURE
         tavg = (tmin + tmax) / 2) %>%
  group_by(date) %>%
  # AVERAGE ALL DC STATIONS
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE)) %>%
  # 7 DAY ROLLING AVERAGE
  mutate(tavg_07da = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmax_07da = rollapply(tmax, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         tmin_07da = rollapply(tmin, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         state = "DC") %>%
  ungroup()
# REORDER COLUMNS
weather_DC <- weather_DC[,c(1,8,2,5,3,6,4,7)]

# BUILD WEATHER DATA
weather_usa <- rbind(weather_usa, weather_DC)


##### PROCESS ALL DATA #####
weather_usa %>%
  arrange(date)
save(weather_usa, file = "rda/weather_usa.rda")




