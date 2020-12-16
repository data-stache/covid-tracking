library(tidyverse)
library(stringr)
library(lubridate)

# CLEAR ENVIRONS
rm(list=ls())

##### PULL POLITICAL DATA ####
slpli <- read.csv("data/slpli-and-party.csv")
governors <- read.csv("data/gov-party.csv")
cook_pvi <- read.csv("data/cook-pvi.csv")
load("rda/covid.rda")
load('rda/pres_results_2020.rda')

##### GOVERNORS #####
# LOWERCASE TEXT
governors$governor_party <- str_replace(governors$governor_party, "REPUBLICAN", "Republican")
governors$governor_party <- str_replace(governors$governor_party, "DEMOCRAT", "Democrat")

# ADD ABBREVIATION
governors <- governors %>%
  mutate(gov_party = case_when(governor_party == "Republican" ~ "R",
                               governor_party == "Democrat" ~ "D"))
save(governors, file = "rda/governors.rda")


##### SLPLI #####
slpli <- slpli %>%
  # MAKE DC DEMOCRAT
  mutate(SLPLI = ifelse(is.na(SLPLI), "Democrat", SLPLI),
         # ADD PARTY ABBREVIATIONS
         slpli_party = case_when(SLPLI == "Republican" ~ "R",
                               SLPLI == "Democrat" ~ "D")) %>%
  rename(slpli = party_by) %>%
  select(state, slpli, slpli_party)
save(slpli, file = "rda/slpli.rda")

##### COOK PVI #####
# ADD STATE ABBREVIATIONS
cook_pvi <- cook_pvi %>%
  rename(state_name = State,
         pvi = PVI)
cook_pvi <- covid %>% 
  filter(date == ymd(20201001)) %>%
  select(state, state_name) %>%
  left_join(cook_pvi)
cook_pvi <- cook_pvi[,c(1,3)]

# FILTER NA
cook_pvi <- cook_pvi %>%
  filter(!is.na(pvi))

cook_pvi$pvi <- str_replace(cook_pvi$pvi, "\\+", " ")
cook_pvi <- cook_pvi %>%
  extract(pvi, c("party", "index"), regex = "^(D|R)\\s(\\d+)") %>%
  mutate(party = ifelse(is.na(party), "I", party),
         index = as.numeric(index),
         index = ifelse(is.na(index), 0, index),
         index = ifelse(party == "D", index * (-1), index)) %>%
  rename(pvi = index,
         pvi_party = party)
save(cook_pvi, file = "rda/cook_pvi.rda")

##### 2020 ELECTION CLEANUP #####
head(pres_results_2020)

# ADD STATE ABBREVIATIONS
pres_results_2020 <- covid %>% 
  filter(date == ymd(20201001)) %>%
  select(state, state_name) %>%
  right_join(pres_results_2020) %>%
  select(state, lean_2020) %>%
  mutate(party_2020 = case_when(lean_2020 > 0 ~ 'R',
                                lean_2020 < 0 ~ 'D'))
  

##### BUILD POLITICAL COVID DATA SET #####
covid_pol <- covid %>%
  filter(!state == "PR") %>%
  left_join(slpli) %>% 
  left_join(governors) %>%
  left_join(cook_pvi) %>%
  left_join(pres_results_2020) %>%
  select(date, day, state, state_name, pop, new_cases, new_tests, new_death, hosp, percent_pos, 
         slpli, slpli_party, pvi, pvi_party, lean_2020, party_2020)

head(covid_pol)

# APPLY CA SLPLI RATING TO DC TO POL DF
dc_ind <- covid_pol$state == "DC"
max_dem <- covid_pol %>%
  filter(slpli_party == "D") %>%
  .$slpli
max_dem <- max(max_dem, na.rm = TRUE)
covid_pol$slpli[dc_ind] <- max_dem

# STATE LEAN AVG
covid_pol <- covid_pol %>%
  mutate(slpli = ifelse(slpli_party == "D", slpli * (-1), slpli), 
         pvi = pvi / 100,
         lean_avg = (pvi + slpli + lean_2020) / 3,
         state_lean = case_when(lean_avg < 0 ~ "D",
                                lean_avg == 0 ~"I",
                                lean_avg > 0 ~ "R")) %>%
  select(date, day, state, state_name, pop, new_cases, new_tests, new_death, hosp, percent_pos, lean_avg, state_lean, 
         slpli, slpli_party, pvi, pvi_party, lean_2020, party_2020)

library(knitr)
covid_pol %>% filter(date == ymd(20201201)) %>% select(state, lean_avg, state_lean) %>% arrange(lean_avg) %>% kable()

save(covid_pol, file = "rda/covid_pol.rda")

# PARTISAN GRAPHING

avg_lean <- covid_pol %>% filter(date == ymd(20201201)) %>% select(state, lean_avg, state_lean) %>% arrange(lean_avg) %>% .$state

covid_pol %>%
  filter(date == ymd(20201201) & lean_avg < 0) %>%
  mutate(state = factor(state, levels = avg_lean)) %>%
  select(state, lean_avg, slpli, pvi, lean_2020) %>%
  gather(metric, rating, lean_avg:lean_2020) %>%
  ggplot(aes(x = state, y = rating, fill = metric)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_y_reverse()

covid_pol %>%
  filter(date == ymd(20201201) & lean_avg > 0) %>%
  mutate(state = factor(state, levels = avg_lean)) %>%
  select(state, lean_avg, slpli, pvi, lean_2020) %>%
  gather(metric, rating, lean_avg:lean_2020) %>%
  ggplot(aes(x = state, y = rating, fill = metric)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip()
  

# CLEAR ENVIRONS
rm(list=ls())
