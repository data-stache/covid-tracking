library(tidyverse)

## READ / LOAD DATA
flu <- read.csv("data/influenza-pnemonia-deaths.csv")
load('rda/covid_us_sum.rda')

## EPI WEEK COVID DEATHS
covid_death <- covid_us_sum %>%
  mutate(week = epiweek(date)) %>%
  group_by(week) %>%
  summarize(covid_deaths = sum(new_death)) %>%
  mutate(year = 2020) %>%
  select(year, week, covid_deaths)

## FLU TABLE CLEANUP
# Coma Stripping
flu$influenza_deaths <- as.numeric(str_replace_all(flu$influenza_deaths, ",", ""))
flu$pnemonia_deaths <- as.numeric(str_replace_all(flu$pnemonia_deaths, ",", ""))

# Build death table
deaths <- flu %>%
  mutate(ip_deaths = influenza_deaths + pnemonia_deaths) %>%
  arrange(year, week) %>%
  select(season, year, week, influenza_deaths, pnemonia_deaths, ip_deaths) %>%
  full_join(covid_death) %>%
  gather(illness, deaths, influenza_deaths:covid_deaths)

# String Cleanup
deaths$illness <- str_replace_all(deaths$illness, "_deaths", "")

# Build Key
deaths <- deaths %>%
  group_by(season, week) %>%
  mutate(key = paste(year, illness, sep = '_'))


##### PLOTTING DEATHS #####
deaths %>%
  filter(illness %in% c('influenza', 'covid')) %>%
  ggplot(aes(x = week, y = deaths, col = key)) +
  geom_line()

deaths %>%
  filter(illness %in% c('influenza', 'covid') & year > 2013) %>%
  group_by(season, illness) %>%
  summarize(deaths = sum(deaths, na.rm = TRUE)) %>%
  ggplot(aes(x = season, y = deaths, fill = illness)) +
  geom_col(position = 'dodge') +
  theme(axis.text.x = element_text(angle = 90))
