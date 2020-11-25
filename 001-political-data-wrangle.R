library(tidyverse)
library(stringr)
library(lubridate)

##### PULL POLITICAL DATA ####
slpli <- read.csv("data/slpli-and-party.csv")
governors <- read.csv("data/gov-party.csv")
cook_pvi <- read.csv("data/cook-pvi.csv")
load("rda/covid.rda")

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
                               SLPLI == "Democrat" ~ "D"))
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

##### BUILD POLITICAL COVID DATA SET #####
covid_pol <- covid %>%
  filter(!state == "PR") %>%
  left_join(slpli) %>% 
  left_join(governors) %>%
  left_join(cook_pvi) %>%
  select(date, day, state, state_name, pop, new_cases, new_tests, new_death, hosp, percent_pos, SLPLI, party_by, slpli_party, governor_party, gov_party, pvi, pvi_party, mask_law, mask_date)
covid_pol

# APPLY CA SLPLI RATING TO DC TO POL DF
dc_ind <- covid_pol$state == "DC"
max_dem <- covid_pol %>%
  filter(SLPLI == "Democrat") %>%
  .$party_by
max_dem <- max(max_dem, na.rm = TRUE)
covid_pol$party_by[dc_ind] <- max_dem

# STATE LEAN AVG
covid_pol <- covid_pol %>%
  mutate(party_by = ifelse(SLPLI == "Democrat", party_by * (-1), party_by), 
         pvi = pvi / 1000,
         lean_avg = (pvi + party_by) / 2,
         state_lean = case_when(lean_avg < 0 ~ "D",
                                lean_avg == 0 ~"I",
                                lean_avg > 0 ~ "R")) %>%
  select(date, day, state, state_name, pop, new_cases, new_tests, new_death, hosp, percent_pos, lean_avg, state_lean, SLPLI, party_by, slpli_party, governor_party,
         gov_party, pvi, pvi_party, mask_law, mask_date)

save(covid_pol, file = "rda/covid_pol.rda")
