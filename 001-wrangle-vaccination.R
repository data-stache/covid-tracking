{
  library(lubridate)
  library(knitr)
  library(zoo)
  library(stringr)
  library(tidyverse)
  library(tidylog)
}

RUN_DATE <- Sys.Date()

vaccinations <- read.csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv')

vaccinations$location[vaccinations$location == 'New York State'] <- 'New York'

populations <- read.csv("data/us-census-population-data.csv")
populations <- populations %>%
  mutate(state = factor(state)) %>%
  select(state, state_name, population)

STATES <- unique(populations$state_name)

vaccinations <- vaccinations %>%
  filter(location %in% STATES) %>%
  select(date, state_name = location, total_vaccinations, people_vaccinated, people_fully_vaccinated, share_doses_used) %>%
  mutate(date = ymd(date)) %>%
  left_join(populations) %>%
  select(state, state_name, population, date, total_vaccinations, people_vaccinated, people_fully_vaccinated, share_doses_used) %>%
  group_by(state) %>%
  mutate_at(c('total_vaccinations', 'people_vaccinated', 'people_fully_vaccinated', 'share_doses_used'), na.approx, na.rm = FALSE) %>%
  arrange(desc(date)) %>%
  # New Vaccinations
  mutate(new_vaccinations = total_vaccinations - lag(total_vaccinations, n = 1, order_by = date),
         new_people_vaccinated = people_vaccinated - lag(people_vaccinated, n = 1, order_by = date),
         new_fully_vaccinated = people_fully_vaccinated - lag(people_fully_vaccinated, n = 1, order_by = date),
         # Raw Rolling 7 Day Average
         new_vaccinations_07da = rollapply(new_vaccinations, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"),
         new_people_vaccinated_07da = rollapply(new_people_vaccinated, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"),
         new_fully_vaccinated_07da = rollapply(new_fully_vaccinated, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"),
         # Per Capita Data
         vaccinations_per_cap = (total_vaccinations / population) * 100000,
         people_vacinated_per_cap = (people_vaccinated / population) * 100000,
         fully_vaccinated_per_cap = (people_fully_vaccinated / population) * 100000,
         # Per Capita Daily New Vaccinations
         new_vaccinations_per_cap = vaccinations_per_cap - lag(vaccinations_per_cap, n = 1, order_by = date),
         new_people_vaccinated_per_cap = people_vacinated_per_cap - lag(people_vacinated_per_cap, n = 1, order_by = date),
         new_fully_vaccinated_per_cap = fully_vaccinated_per_cap - lag(fully_vaccinated_per_cap, n = 1, order_by = date),
         # Per Capita New Vaccinations Rolling 7 Day
         new_vaccinations_per_cap_07da = rollapply(new_vaccinations_per_cap, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"),
         new_people_vaccinated_per_cap_07da = rollapply(new_people_vaccinated_per_cap, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"),
         new_fully_vaccinated_per_cap_07da = rollapply(new_fully_vaccinated_per_cap, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"),
         # Share Populations
         share_used_doses = share_doses_used,
         share_used_doses_07da = rollapply(share_used_doses, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"),
         share_pop_vaccinated = people_vaccinated / population,
         share_pop_fully_vaccinated = people_fully_vaccinated / population) %>%
  select(-share_doses_used)

save(vaccinations, file = 'rda/vaccinations.rda')

names(vaccinations)

vaccinations %>%
  group_by(date) %>%
  summarize(population = sum(population[1]),
            total_vaccinations = sum(total_vaccinations),
            people_vaccinated = sum(people_vaccinated),
            people_fully_vaccinated = sum(people_fully_vaccinated),
            new_vaccinations = sum(new_vaccinations),
            new_people_vaccinated = sum(new_people_vaccinated),
            new_fully_vaccinated = sum(new_fully_vaccinated)) %>%
  arrange(desc(date)) %>%
  mutate(share_pop_vaccinated = people_vaccinated / population,
         share_pop_fully_vaccinated = people_fully_vaccinated / population)
            
  

# Vaccination Last Update ------------------------------------------------------
max(vaccinations$date)



# Clear ------------------------------------------------------------------------
rm(list=ls())

















