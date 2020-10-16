# LOAD DATE
load("rda/covid_us_sum.rda")

# CASES THIS HIGH
covid_us_sum %>%
  filter(new_cases >= covid_us_sum$new_cases[1]) %>% select(date, new_cases)

# DAILY DEATHS THIS HIGH
covid_us_sum %>%
  filter(new_death >= covid_us_sum$new_death[1]) %>% select(date, new_death)

# 7 DAY AVG DEATHS THIS HIGH
covid_us_sum %>%
  filter(new_death_07da >= covid_us_sum$new_death_07da[1]) %>% select(date, new_death_07da)

# % POSITIVE ABOVE ##
p <- .06
covid_us_sum %>%
  filter(percent_pos >= p) %>% select(date, percent_pos)
