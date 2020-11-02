# LOAD DATE
load("rda/covid_us_sum.rda")
load("rda/covid.rda")
load("rda/ind_2w.rda")

# COVID CASES TOP 10
covid_us_sum %>%
  arrange(desc(new_cases)) %>% slice_head(n=10) %>% select(date, day, new_cases)

# COVID CASES TOP 10
covid_us_sum %>%
  arrange(desc(new_cases_07da)) %>% slice_head(n=10) %>% select(date, day, new_cases_07da)

# CASES THIS HIGH
covid_us_sum %>%
  filter(new_cases >= covid_us_sum$new_cases[1]) %>% select(date, new_cases)

# 7 DAY AVERAGE OF CASES THIS HIGH
covid_us_sum %>%
  filter(new_cases_07da >= covid_us_sum$new_cases_07da[1]) %>% select(date, new_cases_07da)

# DAILY DEATHS THIS HIGH
covid_us_sum %>%
  filter(new_death >= covid_us_sum$new_death[1]) %>% select(date, new_death)

# 7 DAY AVG DEATHS THIS HIGH
covid_us_sum %>%
  filter(new_death_07da >= covid_us_sum$new_death_07da[1]) %>% select(date, new_death_07da)

# % POSITIVE ABOVE ##
covid_us_sum %>%
  filter(percent_pos >= covid_us_sum$percent_pos[1]) %>% select(date, percent_pos)

# WORST 7 DAY CASE AVERAGES
covid %>%
  group_by(state) %>%
  filter(new_cases_percap == max(new_cases_percap)) %>%
  select(state, date, new_cases_percap_07da) %>%
  filter(date >= ind_2w) %>%
  arrange(desc(date))
  
