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
         r_mean = mean)

save(transmission, file = 'rda/transmission.rda')
rm(list=ls())