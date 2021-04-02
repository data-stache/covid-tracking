# Load Libraries ---------------------------------------------------------------
{
  library(tidyverse)
  library(stringr)
  library(lubridate)
  library(tidylog)
}



# Pull Politicl Data -----------------------------------------------------------
load("rda/covid_county.rda")
election_2020_county <- read.csv('https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-20/master/2020_US_County_Level_Presidential_Results.csv')



# Wrangle Pres Data ------------------------------------------------------------
covid_election_2020_county <- election_2020_county %>%
  rename(state = state_name, FIPS = county_fips) %>%
  mutate(margin_2020 = (votes_gop - votes_dem) / total_votes,
         party_2020 = case_when(margin_2020 > 0 ~ 'R',
                                margin_2020 < 0 ~ 'D')) %>%
  select(state, FIPS, margin_2020, party_2020) %>%
  left_join(covid_county) %>%
  filter(!is.na(date)) %>%
  arrange(desc(date)) %>%
  select(state, county, FIPS, date, margin_2020, party_2020, population:new_deaths_percap_07da)

save(covid_election_2020_county, file = 'rda/covid_election_2020_county.rda')

covid_election_2020_county %>%
#  filter(date == covid_election_2020_county$date[1]) %>%
  group_by(date, party_2020) %>%
  summarize(population = sum(population),
            total_cases = sum(total_cases),
            total_deaths = sum(total_deaths)) %>%
  mutate(cases_percap = (total_cases / population) * 100000,
         deaths_percap = (total_deaths / population) * 100000) %>%
  arrange(desc(date)) %>%
  ggplot(aes(x = date, y = deaths_percap, color = party_2020)) +
  geom_line()
  
  