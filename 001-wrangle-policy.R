# CLEAR ENVIRONS
rm(list=ls())

library(tidyverse)
library(lubridate)
library(tidylog)

policy <- read.csv('data/usa-covid-policy-by-state.csv') %>%
  mutate(Date = ymd(Date)) %>%
  arrange(desc(Date)) %>%
  filter(!RegionName == '' & !RegionName == 'Virgin Islands') %>%
  select(RegionCode, Date,
         StringencyIndexForDisplay, GovernmentResponseIndexForDisplay, ContainmentHealthIndexForDisplay, EconomicSupportIndexForDisplay) %>%
  rename(state = RegionCode,
         date = Date,
         stringency_index = StringencyIndexForDisplay,
         government_index = GovernmentResponseIndexForDisplay,
         containment_index = ContainmentHealthIndexForDisplay,
         economic_index = EconomicSupportIndexForDisplay) %>%
  mutate(mean_index = (stringency_index + government_index + containment_index + economic_index) / 4,
         state = str_replace(state, 'US_', ''))

save(policy, file = 'rda/policy.rda')
rm(list=ls())