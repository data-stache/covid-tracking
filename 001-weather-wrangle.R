library(tidyverse)

##### WRANGLE MOBILITY COVID DATA ####
weather <- read.csv("data/weather/de_weather.csv")

names(weather)

weather <- weather %>%
  rename(station = STATION,
         name = NAME,
         date = DATE,
         avg_t = TAVG,
         hi = TMAX,
         lo = TMIN) %>%
  filter(!is.na(avg_t) & !is.na(hi) & !is.na(lo)) %>%
  mutate(date = ymd(date)) %>%
  group_by(date) %>%
  summarise(avg_T = mean(avg_t),
            avg_HI = mean(hi),
            avg_LO = mean(lo)) %>%
  mutate(avg_T_07da = rollapply(avg_T, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         avg_HI_07da = rollapply(avg_HI, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         avg_LO_07da = rollapply(avg_LO, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"))
save(weather, file = "rda/de_weather.rda")