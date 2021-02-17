# CLEAR ENVIRONS
rm(list=ls())

library(tidyverse)
library(lubridate)
library(tidylog)

transmission <- read.csv('data/r-transmission.csv') %>%
  mutate(date = ymd(date)) %>%
  select(date,
         region,
         mean) %>%
  arrange(desc(date)) %>%
  rename(state = region,
         r_mean = mean) %>%
  group_by(state) %>%
  arrange(date) %>%
  mutate(r_mean_07da = rollapply(r_mean, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right")) %>%
  ungroup() %>%
  arrange(desc(date))

save(transmission, file = 'rda/transmission.rda')
rm(list=ls())
