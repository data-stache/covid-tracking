library(tidyverse)
library(stringr)

##### PULL POLITICAL DATA ####
slpli <- read.csv("data/slpli-and-party.csv")
governors <- read.csv("data/gov-party.csv")
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

# BUILD POLITICAL COVID DATA SET
covid_pol <- covid %>% 
  left_join(slpli) %>% 
  left_join(governors) %>%
  select(date, state, state_name, pop, new_cases, new_tests, new_death, hosp, percent_pos, SLPLI, party_by, slpli_party, governor_party, gov_party, mask_law, mask_date)
covid_pol
save(covid_pol, file = "rda/covid_pol.rda")
  
