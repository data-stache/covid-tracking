# CLEAR ENVIRONS
rm(list=ls())

# Load Libraries
{
  library(tidyverse, quietly = TRUE)
  library(lubridate, quietly = TRUE)
  library(tidylog, quietly = TRUE)
  library(broom, quietly = TRUE)
}

load('rda/weather_usa.rda')
load('rda/mobility.rda')
load('rda/policy.rda')
load('rda/transmission.rda')
load('rda/covid.rda')

STATES <- read.csv("data/us-census-population-data.csv") %>%
  select(state, state_name)

# Weather Chage
CHANGE <- weather_usa %>%
  group_by(state) %>%
  mutate(tavg_change = (tavg_07da - lead(tavg_07da, n = 14, order_by = desc(date))) / lead(tavg_07da, n = 14, order_by = desc(date))) %>%
  left_join(STATES) %>%
  select(date, state_name, state, tavg_change) %>%
  arrange(desc(date))


# Add Mobility Change
CHANGE <- mobility %>%
  group_by(state_name) %>%
  mutate(retail_recreation_scaled = (retail_recreation_07da - min(retail_recreation_07da, na.rm = TRUE)) / (max(retail_recreation_07da, na.rm = TRUE) - min(retail_recreation_07da, na.rm = TRUE)),
         transit_scaled = (transit_07da - min(transit_07da, na.rm = TRUE)) / (max(transit_07da, na.rm = TRUE) - min(transit_07da, na.rm = TRUE)),
         workplace_scaled = (workplace_07da - min(workplace_07da, na.rm = TRUE)) / (max(workplace_07da, na.rm = TRUE) - min(workplace_07da, na.rm = TRUE)),
         retail_change = (retail_recreation_scaled - lead(retail_recreation_scaled, n = 14, order_by = desc(date))) / lead(retail_recreation_scaled, n = 14, order_by = desc(date)),
         transit_change = (transit_scaled - lead(transit_scaled, n = 14, order_by = desc(date))) / lead(transit_scaled, n = 14, order_by = desc(date)),
         workplace_change = (workplace_scaled - lead(workplace_scaled, n = 14, order_by = desc(date))) / lead(workplace_scaled, n = 14, order_by = desc(date))) %>%
  select(state_name, date, retail_change, transit_change, workplace_change) %>%
  left_join(STATES) %>%
  arrange(desc(date)) %>%
  select(date, state_name, state, retail_change, transit_change, workplace_change) %>%
  left_join(CHANGE)

# DC Temp Fix
DC_FIX <- CHANGE %>%
  filter(state %in% c('MD', 'VA', 'DC') & date > ymd(20201031)) %>%
  group_by(date) %>%
  mutate(tavg_change = ifelse(is.na(tavg_change), mean(tavg_change, na.rm = TRUE), tavg_change)) %>%
  filter(state == 'DC') %>%
  .$tavg_change

CHANGE$tavg_change[CHANGE$state == 'DC' & CHANGE$date > ymd(20201031)] <- DC_FIX


# Add Policy Score Change
CHANGE <- policy %>%
  left_join(STATES) %>%
  group_by(state) %>%
  mutate(index_change = (mean_index - lead(mean_index, n = 14, order_by = desc(date))) / lead(mean_index, n = 14, order_by = desc(date))) %>%
  select(state, state_name, date, index_change) %>%
  left_join(CHANGE) %>%
  arrange(desc(date)) %>%
  drop_na()


# Add Transmission Change
CHANGE <- transmission %>%
  mutate(r_change = (r_mean_07da - lead(r_mean_07da, n = 14, order_by = desc(date))) / lead(r_mean_07da, n = 14, order_by = desc(date))) %>%
  select(-r_mean, -r_mean_07da) %>%
  left_join(CHANGE) %>%
  arrange(desc(date))


# Add Covid
CHANGE <- covid %>%
  select(state, state_name, date, new_cases_07da) %>%
  group_by(state) %>%
  mutate(covid_change = (new_cases_07da - lead(new_cases_07da, n = 14, order_by = desc(date))) / lead(new_cases_07da, n = 14, order_by = desc(date))) %>%
  select(-new_cases_07da) %>%
  left_join(CHANGE) %>%
  drop_na() %>%
  ungroup()


head(CHANGE)

dat <- CHANGE %>%
  select(covid_change:tavg_change)

dat <- dat[!is.infinite(rowSums(dat)),]

tidy(lm(covid_change ~ ., data = dat), conf.int = TRUE) %>%
  filter(p.value < .05)









