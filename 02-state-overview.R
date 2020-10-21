# DAILY LOOKUP TOTAL
load("rda/covid.rda")
load("rda/ind_tywm_date.rda")
load("rda/covid_state_growth.rda")
load("rda/covid_state_zones.rda")
options(digits = 3)

# PICK A STATE
st <- "CT"

covid %>%
  select(date, state, new_cases, new_tests, percent_pos, new_death, hosp) %>%
  filter(state == st) %>%
  filter(date %in% ind_tywm_date)

# DAILY LOOKUP PER CAPITA
covid %>%
  select(date, state, new_cases_percap, new_tests_percap, percent_pos, new_death_percap, hosp_percap) %>%
  filter(state == st) %>%
  filter(date %in% ind_tywm_date)

# NEW CASE PER CAPITA LAST 7 DAYS

merge(covid_state_growth, covid_state_zones, by = c("state", "sum_cases_percap", "percent_pos")) %>%
  filter(state == st) %>%
  select(state_name, sum_cases_percap, sum_tests_percap, percent_pos, sum_death_percap, sum_new_hosp, cases_zone, percent_zone)
