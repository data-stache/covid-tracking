# CLEAR ENVIRONS
rm(list=ls())


# Load Libraries ---------------------------------------------------------------
{
  library(tidyverse)
  library(lubridate)
  library(zoo)
  library(tidylog)
}



# Read Data --------------------------------------------------------------------
# Confirmed Cases
dat_cases <- read.csv(file = "data/covid-confirmed-cases-johns-hopkins.csv")

dat_cases <- dat_cases %>%
  gather(date, cases, -UID:-Combined_Key) %>%
  mutate(date = gsub('X', '', date),
         date = mdy(date))


# Confirmed Deaths
dat_deaths <- read.csv(file = "data/covid-confirmed-deaths-johns-hopkins.csv")

dat_deaths <- dat_deaths %>%
  gather(date, deaths, -UID:-Population) %>%
  mutate(date = gsub('X', '', date),
         date = mdy(date))



# Merge Data -------------------------------------------------------------------
covid <- dat_cases %>%
  left_join(dat_deaths,by = c('date', 'Province_State', 'Admin2')) %>%
  mutate(day = weekdays(date)) %>%
  select(date, day, state = Province_State, county = Admin2, FIPS = FIPS.x, lat = Lat.x, long = Long_.x, population = Population, total_cases = cases, total_deaths = deaths)

omit <- c('Diamond Princess', 'Grand Princess', 'American Samoa', 'Northern Mariana Islands', 'Guam', 'Virgin Islands')

# New Cases / New Deaths
covid <- covid %>%
  filter(!state %in% omit) %>%
  group_by(state, county) %>%
  mutate(new_cases = total_cases - lag(total_cases, n = 1, order_by = date),
         new_deaths = total_deaths - lag(total_deaths, n = 1, order_by = date)) # NEW CASES


# County Level -----------------------------------------------------------------
covid_county <- covid %>%
  group_by(state, county) %>%
  mutate(new_cases_07da = rollapply(new_cases, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         new_deaths_07da = rollapply(new_deaths, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         new_cases_percap = (new_cases / population) * 100000,
         new_deaths_percap = (new_deaths / population) * 100000,
         new_cases_percap_07da = rollapply(new_cases_percap, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         new_deaths_percap_07da = rollapply(new_deaths_percap, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right")) %>%
  ungroup()



# State Level -----------------------------------------------------------------
covid_state <- covid %>%
  group_by(date, day, state) %>%
  summarize(population = sum(population, na.rm = TRUE),
            total_cases = sum(total_cases, na.rm = TRUE),
            total_deaths = sum(total_deaths, na.rm = TRUE),
            new_cases = sum(new_cases, na.rm = TRUE),
            new_deaths = sum(new_deaths, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(state) %>%
  mutate(new_cases_07da = rollapply(new_cases, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         new_deaths_07da = rollapply(new_deaths, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         new_cases_percap = (new_cases / population) * 100000,
         new_deaths_percap = (new_deaths / population) * 100000,
         new_cases_percap_07da = rollapply(new_cases_percap, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         new_deaths_percap_07da = rollapply(new_deaths_percap, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right")) %>%
  ungroup()



# National ---------------------------------------------------------------------
covid_national <- covid %>%
  group_by(date, day) %>%
  summarize(population = sum(population, na.rm = TRUE),
            total_cases = sum(total_cases, na.rm = TRUE),
            total_deaths = sum(total_deaths, na.rm = TRUE),
            new_cases = sum(new_cases, na.rm = TRUE),
            new_deaths = sum(new_deaths, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(new_cases_07da = rollapply(new_cases, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         new_deaths_07da = rollapply(new_deaths, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         new_cases_percap = (new_cases / population) * 100000,
         new_deaths_percap = (new_deaths / population) * 100000,
         new_cases_percap_07da = rollapply(new_cases_07da, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         new_deaths_percap_07da = rollapply(new_deaths_07da, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right")) %>%
  ungroup()



save(covid_county, file = 'rda/covid_county.rda')
save(covid_state, file = 'rda/covid_state.rda')
save(covid_national, file = 'rda/covid_national.rda')



# Build Date Index -------------------------------------------------------------
dates <- rev(unique(covid$date))
ind_tywm <- c(1, 2, 8, 29) # TODAY / YESTERDAY / LAST WEEK / LAST MONTH
save(ind_tywm, file = "rda/ind_tywm.rda")

ind_tywm_date <- dates[ind_tywm] # PULL DATES OF T/Y/LW/LM
save(ind_tywm_date, file = "rda/ind_tywm_date.rda")

ind_month <- dates[29] # 4 WEEKS AGO
save(ind_month, file = "rda/ind_month.rda")

ind_90day <- dates[91] # 90 DAYS AGO
save(ind_90day, file = "rda/ind_90day.rda")

ind_wk <- dates[8] # 7 DAYS AGO
save(ind_wk, file = "rda/ind_wk.rda")

ind_2w <- dates[15] # 7 DAYS AGO
save(ind_2w, file = "rda/ind_2w.rda")

ind_tdy <- dates[1] # TODAY
save(ind_tdy, file = "rda/ind_tdy.rda")

ind_yale_rtrn <- ymd(20200824) # DATE YALE UNDERGRADS BEGIN TO RETURN
save(ind_yale_rtrn, file = "rda/ind_yale_rtrn.rda")

x_time <- c(1,8,15,22,29,36)
ind_x_time <- dates[x_time]

# 3 MONTH LIMIT SET
xlim1 <- dates[91]
xlim2 <- ind_tdy
ind_xlim_3m <- c(xlim1, xlim2)
save(ind_xlim_3m, file = "rda/ind_xlim_3m.rda")



# Growth Charts ----------------------------------------------------------------
# County
covid_county_growth <- covid_county %>%
  filter(date > ind_2w) %>%
  mutate(weeks = ifelse(date > ind_wk, "Last 7 Days", "Two Weeks Ago")) %>%
  group_by(state, county, weeks) %>%
  summarize(new_cases_percap = sum(new_cases_percap, na.rm = TRUE),
            new_deaths_percap = sum(new_deaths_percap, na.rm = TRUE)) %>%
  ungroup()
save(covid_county_growth, file = "rda/covid_county_growth.rda")

# State
covid_state_growth <- covid_state %>%
  filter(date > ind_2w) %>%
  mutate(weeks = ifelse(date > ind_wk, "Last 7 Days", "Two Weeks Ago")) %>%
  group_by(state, weeks) %>%
  summarize(new_cases_percap = sum(new_cases_percap, na.rm = TRUE),
            new_deaths_percap = sum(new_deaths_percap, na.rm = TRUE)) %>%
  ungroup()
save(covid_state_growth, file = "rda/covid_state_growth.rda")

# National
covid_national_growth <- covid_national %>%
  filter(date > ind_2w) %>%
  mutate(weeks = ifelse(date > ind_wk, "Last 7 Days", "Two Weeks Ago")) %>%
  group_by(weeks) %>%
  summarize(new_cases = sum(new_cases, na.rm = TRUE),
            new_deaths = sum(new_deaths, na.rm = TRUE)) %>%
  ungroup()
save(covid_national_growth, file = "rda/covid_national_growth.rda")



# County Order Indexes ---------------------------------------------------------
dat_county <- covid_county %>%
  filter(date == ind_tdy)

case_order_county <- dat_county %>%
  filter(!is.infinite(new_cases_percap_07da)) %>%
  select(state, county, new_cases_percap_07da) %>%
  arrange(desc(new_cases_percap_07da)) %>%
  mutate(rank = seq(1, length(dat_county$new_cases_percap_07da[!is.infinite(dat_county$new_cases_percap_07da)]), 1))
save(case_order_county, file = 'rda/case_order_county.rda')

death_order_county <- dat_county %>%
  filter(!is.infinite(new_deaths_percap_07da)) %>%
  select(state, county, new_deaths_percap_07da) %>%
  arrange(desc(new_deaths_percap_07da)) %>%
  mutate(rank = seq(1, length(dat_county$new_deaths_percap_07da[!is.infinite(dat_county$new_deaths_percap_07da)]), 1))
save(death_order_county, file = 'rda/death_order_county.rda')



# State Order Indexes ----------------------------------------------------------
dat_state <- covid_state %>%
  filter(date == ind_tdy)

case_order_state <- dat_state %>%
  select(state, new_cases_percap_07da) %>%
  arrange(desc(new_cases_percap_07da)) %>%
  mutate(rank = seq(1, length(dat_state$date), 1))
save(case_order_state, file = 'rda/case_order_state.rda')

death_order_state <- dat_state %>%
  select(state, new_deaths_percap_07da) %>%
  arrange(desc(new_deaths_percap_07da)) %>%
  mutate(rank = seq(1, length(dat_state$date), 1))
save(death_order_state, file = 'rda/death_order_state.rda')



# JOHNS HOPKINS DATA LAST UPDATED
ind_tdy

# CLEAR ENVIRONS
rm(list=ls())

