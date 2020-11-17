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
  select(date, new_cases, new_tests, percent_pos, new_death, new_hosp) %>%
  filter(date %in% ind_tywm_date) %>%
  kable()

covid_us_sum %>%
  select(date, new_cases_07da, new_tests_07da, percent_pos_07da, new_death_07da, new_hosp_07da) %>%
  filter(date %in% ind_tywm_date) %>%
  kable()

covid_us_growth %>%
  kable()
