##### DATA LOAD #####
library(lubridate)
library(broom)
library(tidyverse)
load("rda/covid_pol.rda")
load("rda/slpli.rda")

pol_party <- c("blue", "dark red")
party_3 <- c("blue", "dark green", "dark red")

# APPLY CA SLPLI RATING TO DC TO POL DF
dc_ind <- covid_pol$state == "DC"
max_dem <- covid_pol %>%
  filter(SLPLI == "Democrat") %>%
  .$party_by
max_dem <- max(max_dem, na.rm = TRUE)
covid_pol$party_by[dc_ind] <- max_dem



##### PROCESSING #####
covid_pol <- covid_pol %>%
  # REMOVE PUERTO RICO
  filter(!state == "PR") %>%
  # ALTER PARTY BY TO (-) DEM and (+) REP
  mutate(party_by = ifelse(SLPLI == "Democrat", party_by * (-1), party_by),
         party_by = party_by * 100)


##### RESEARCH SLPLI #####
month_death_pc <- covid_pol %>%
  mutate(month = month(date)) %>%
  select(date, month, state, state_name, pop, new_death, SLPLI) %>%
  group_by(SLPLI, month) %>%
  summarise(death = sum(new_death) / sum(unique(pop)) * 100000)
month_death_pc

total_death_pc <- covid_pol %>%
  select(date, state, state_name, pop, new_death, SLPLI) %>%
  group_by(SLPLI) %>%
  summarise(death = sum(new_death) / sum(unique(pop)) * 100000)
total_death_pc

month_death_pc %>%
  ggplot(aes(x = month, y = death, fill = SLPLI)) +
  geom_col(position = "dodge") +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  scale_fill_manual(values = pol_party)

month_death_pc %>%
  ggplot() +
  geom_bar(aes(x = SLPLI, y = death), stat = "identity") +
  scale_fill_manual(values = pol_party)


##### RESEARCH PVI #####
month_death_pc <- covid_pol %>%
  mutate(month = month(date)) %>%
  select(date, month, state, state_name, pop, new_death, pvi_party) %>%
  group_by(pvi_party, month) %>%
  summarise(death = sum(new_death) / sum(unique(pop)) * 100000)
month_death_pc

total_death_pc <- covid_pol %>%
  select(date, state, state_name, pop, new_death, pvi_party) %>%
  group_by(pvi_party) %>%
  summarise(death = sum(new_death) / sum(unique(pop)) * 100000)
total_death_pc

month_death_pc %>%
  ggplot(aes(x = month, y = death, fill = pvi_party)) +
  geom_col(position = "dodge") +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  scale_fill_manual(values = party_3)

month_death_pc %>%
  ggplot() +
  geom_bar(aes(x = pvi_party, y = death), stat = "identity") +
  scale_fill_manual(values = party_3)

