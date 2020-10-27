library(tidyverse)
library(stringr)
##### PULL POLITICAL DATA ####
slpli <- read.csv("data/slpli-and-party.csv")
governors <- read.csv("data/gov-party.csv")

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
         (gov_party = case_when(SLPLI == "Republican" ~ "R",
                               SLPLI == "Democrat" ~ "D")))
save(slpli, file = "rda/slpli.rda")
  
