##### PULL IN POPULATION DATA ####
populations <- read.csv("data/us-census-population-data.csv")
populations <- populations %>%
  mutate(state = factor(state))
save(populations, file = "rda/populations.rda")

##### PULL IN MASK MANDATE DATA ####
masks <- read.csv("data/mask-mandate.csv")
masks <- masks %>%
  mutate(state = factor(state))
masks$date_issued <- ymd(masks$date_issued)
masks$mandate_expired <- ymd(masks$mandate_expired)
masks <- masks %>%
  mutate(mask_law = mandate,
         mask_date = date_issued) %>%
  select(state, mask_law, mask_date)
save(populations, file = "rda/masks.rda")


##### WRANGLE COVID DATA ####
covid <- read.csv("data/covid-daily-report-by-state.csv")
covid <- covid %>%
  mutate(state = factor(state))
covid <- merge(covid, populations, by = "state")

covid <- data.frame(date = covid$date,
                    state_name = covid$state_name,
                    state = covid$state, 
                    pop = covid$population,
                    new_cases = covid$positiveIncrease, 
                    new_tests = covid$totalTestResultsIncrease, 
                    new_death = covid$deathIncrease)

# PER 100K
covid <- covid %>%
  mutate(percent_pos = new_cases / new_tests,
         new_cases_percap = new_cases / pop * 100000,
         new_tests_percap = new_tests / pop * 100000,
         new_death_percap = new_death / pop * 100000,
         # FORMAT DATE
         date = ymd(date)) %>%
  # ARRANGE BY DATE (MOST RECENT -> OLDEST)
  arrange(desc(date))

# 7 DAY ROLLING AVERAGE
covid <- covid %>%
  group_by(state) %>% 
  mutate(new_cases_07da = rollapply(new_cases, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"),
         new_tests_07da = rollapply(new_tests, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"),
         new_death_07da = rollapply(new_death, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"),
         new_cases_percap_07da = rollapply(new_cases_percap, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"),
         new_tests_percap_07da = rollapply(new_tests_percap , width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"),
         new_death_percap_07da = rollapply(new_death_percap, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"),
         percent_pos_07da = rollapply(percent_pos, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left")) %>% 
  ungroup()

# MASK LAW?
covid <- merge(covid, masks, by = "state") %>%
  arrange(desc(date))
save(covid, file = "rda/covid.rda")


##### BUILD DATE INDEX #####
dates <- unique(covid$date)
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


##### SUMMARIZE US TOTAL DATA ####

covid_us_sum <- covid %>%
group_by(date) %>%
  summarise(new_cases = sum(new_cases),
            new_tests = sum(new_tests),
            percent_pos = round(new_cases / new_tests, 3),
            new_death = sum(new_death)) %>%
  # ARRANGE BY DATE (MOST RECENT -> OLDEST)
  arrange(desc(date)) %>%
  # ADD 7 DAY AVERAGES
  mutate(new_cases_07da = rollapply(new_cases, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"),
         new_tests_07da = rollapply(new_tests, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"),
         percent_pos_07da = rollapply(percent_pos, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"),
         new_death_07da = rollapply(new_death, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"))
save(covid_us_sum, file = "rda/covid_us_sum.rda")

covid_us_growth <- covid_us_sum %>%
  filter(date >= ind_2w) %>%
  mutate(weeks = ifelse(date >= ind_wk, "Last 7 Days", "Two Weeks Ago")) %>%
  group_by(weeks) %>%
  summarize(sum_new_cases = sum(new_cases),
            sum_new_tests = sum(new_tests),
            mean_percent_pos = mean(percent_pos, na.rm = TRUE),
            sum_new_death = sum(new_death)) %>%
  ungroup()
save(covid_us_growth, file = "rda/covid_us_growth.rda")


##### SUMMARIZE STATES DATA #####

# SUMMARISE NEW CASES, TESTS, DEATHS
covid_state_growth <- covid %>%
  filter(date >= ind_wk) %>%
  group_by(state, state_name) %>%
  summarize(sum_cases_percap = sum(new_cases_percap),
            sum_tests_percap = sum(new_tests_percap),
            percent_pos = mean(percent_pos, na.rm = TRUE),
            sum_death_percap = sum(new_death_percap)) %>%
  ungroup()
save(covid_state_growth, file = "rda/covid_state_growth.rda")

# STATE INDEX LISTS BY CASES / TESTS / DEATHS
ind_new_case_state <- c(order(desc(covid_state_growth$sum_cases_percap)))
save(ind_new_case_state, file = "rda/ind_new_case_state.rda")

ind_new_test_state <- c(order(desc(covid_state_growth$sum_tests_percap)))
save(ind_new_test_state, file = "rda/ind_new_test_state.rda")

ind_new_death_state <- c(order(desc(covid_state_growth$sum_death_percap)))
save(ind_new_death_state, file = "rda/ind_new_death_state.rda")


##### MASKS and ZONES GREEN / YELLOW / RED #####

ind_no_masks <- masks %>%
  mutate(state = as.character(state)) %>%
  filter(mask_law == "NO") %>%
  .$state
save(ind_no_masks, file = "rda/ind_no_masks.rda")

# PERCENT POS FUNCTION
fct_positive_zone <- function(x) { 
  if(x < .05) {
    print("green")
  } else if (x >= .05 & x < .1) {
    print("yellow")
  } else if (x >= .1) {
    print("red")
  }}

# CASES FUNCTION
fct_cases_zone <- function(x) { 
  if(x < 10) {
    print("green")
  } else if (x >= 10 & x < 100) {
    print("yellow")
  } else if (x >= 100) {
    print("red")
  }}

# ZONE DATA FRAME
covid_state_zones <- covid %>%
  filter(date >= ind_wk) %>%
  group_by(state) %>%
  summarize(sum_cases_percap = sum(new_cases_percap),
            percent_pos = mean(percent_pos, na.rm = TRUE),
            cases_zone = fct_cases_zone(sum_cases_percap),
            percent_zone = fct_positive_zone(percent_pos)) %>%
  ungroup()
save(covid_state_zones, file = "rda/covid_state_zones.rda")

ind_red_perpos <- covid_state_zones %>%
  mutate(state = as.character(state)) %>%
  filter(percent_zone == "red") %>%
  .$state
save(ind_red_perpos, file = "rda/ind_red_perpos.rda")

ind_red_cases <- covid_state_zones %>%
  mutate(state = as.character(state)) %>%
  filter(cases_zone == "red") %>%
  .$state
save(ind_red_cases, file = "rda/ind_red_cases.rda")