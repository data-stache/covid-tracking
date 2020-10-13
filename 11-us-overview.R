# LOAD DATA
load("rda/covid_us_sum.rda")
load("rda/ind_red_perpos.rda")
load("rda/ind_red_cases.rda")
load("rda/covid_us_growth.rda")
load("rda/ind_tywm_date.rda")
load("rda/covid_state_zones.rda")

#### US DAILY STATS ####
length(ind_red_perpos)

length(ind_red_cases)

covid_us_sum %>%
  select(date, new_cases, new_tests, percent_pos, new_death) %>%
  filter(date %in% ind_tywm_date)

covid_us_growth
