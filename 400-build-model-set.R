load('rda/covid.rda')
load('rda/transmission.rda')
load('rda/mobility.rda')
load('rda/weather_usa.rda')
load('rda/covid_pol.rda')
load('rda/policy.rda')
load('rda/theme_DataStache.rda')

head(transmission)
head(mobility)
head(weather_usa)
head(covid_pol)
head(policy)
head(covid)

##### BUILD SET #####

# Covid Data Set
dat <- covid %>%
  select(state, state_name, date, pop, new_cases, new_tests, new_death, hosp)

# Add R
dat <- dat %>%
  left_join(transmission)

# Add Mobility
dat_mob <- mobility %>%
  select(state_name, date, retail_recreation, grocery_pharmacy, parks, transit, workplace, residential)

dat <- dat %>%
  left_join(dat_mob)

# Add Weather
dat_weather <- weather_usa %>%
  select(date, state, tavg)

dat <- dat %>%
  left_join(dat_weather)

# Add State Lean
dat_lean <- covid_pol %>%
  select(date, state, lean_avg)

dat <- dat %>%
  left_join(dat_lean)

# Add State Policy
dat_pol <- policy %>%
  group_by(state, date) %>%
  mutate(mean_3 = (stringency_index + government_index + containment_index) / 3)

dat <- dat %>%
  left_join(dat_pol)

head(dat)


##### EXPERIMENT #####
library(ggplot2)
library(gridExtra)
library(ggrepel)

names(dat)

mth <- 'Nov'

# TRANSMISSION FACTOR
dat %>%
  filter(date >= ymd(20200301)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(state, month) %>%
  summarize(cases = sum(new_cases) / unique(pop) * 100000,
            transmission = mean(r_mean, na.rm = TRUE)) %>%
  ggplot(aes(x = transmission, y = cases, label = state)) +
  geom_vline(xintercept = 1, size = .5, col = "40grey") +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(color = "dark blue", alpha = .5) +
  geom_smooth(method = "lm") +
  theme_DataStache() +
  ggtitle("Does R Impact New Case Load") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "Lean based on FiveThirtyEight SLPLI and Cook PVI") +
  facet_wrap(. ~ month)

# MOBILITY - WORKPLACE FACTOR
dat %>%
  filter(date >= ymd(20200301)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(state, month) %>%
  summarize(cases = sum(new_cases) / unique(pop) * 100000,
            work = mean(workplace, na.rm = TRUE)) %>%
  ggplot(aes(x = work, y = cases, label = state)) +
#  geom_vline(xintercept = 1, size = .5, col = "40grey") +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(color = "dark blue", alpha = .5) +
  geom_smooth(method = "lm") +

  theme_DataStache() +
  ggtitle("Does Workplace Mobility Impact New Case Load") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "Lean based on FiveThirtyEight SLPLI and Cook PVI") +
  facet_wrap(. ~ month)

# MOBILITY - RETAIL AND RECREATION FACTOR
dat %>%
  filter(date >= ymd(20200301)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(state, month) %>%
  summarize(cases = sum(new_cases) / unique(pop) * 100000,
            rec = mean(retail_recreation, na.rm = TRUE)) %>%
  ggplot(aes(x = rec, y = cases, label = state)) +
  #  geom_vline(xintercept = 1, size = .5, col = "40grey") +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(color = "dark blue", alpha = .5) +
  geom_smooth(method = "lm") +

  theme_DataStache() +
  ggtitle("Does Retail and Recreation Mobility Impact New Case Load") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "Lean based on FiveThirtyEight SLPLI and Cook PVI") +
  facet_wrap(. ~ month)

# MOBILITY - RESIDENTIAL FACTOR
dat %>%
  filter(date >= ymd(20200301)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(state, month) %>%
  summarize(cases = sum(new_cases) / unique(pop) * 100000,
            res = mean(residential, na.rm = TRUE)) %>%
  ggplot(aes(x = res, y = cases, label = state)) +
  #  geom_vline(xintercept = 1, size = .5, col = "40grey") +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(color = "dark blue", alpha = .5) +
  geom_smooth(method = "lm") +

  theme_DataStache() +
  ggtitle("Does Residential Mobility Impact New Case Load") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "Lean based on FiveThirtyEight SLPLI and Cook PVI") +
  facet_wrap(. ~ month)

# WEATHER FACTOR
dat %>%
  filter(date >= ymd(20200301)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(state, month) %>%
  summarize(cases = sum(new_cases) / unique(pop) * 100000,
            temp = mean(tavg, na.rm = TRUE)) %>%
  ggplot(aes(x = temp, y = cases, label = state)) +
  #  geom_vline(xintercept = 1, size = .5, col = "40grey") +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(color = "dark blue", alpha = .5) +
  geom_smooth(method = "lm") +

  theme_DataStache() +
  ggtitle("Does Average Temperatrue Impact New Case Load") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "Lean based on FiveThirtyEight SLPLI and Cook PVI") +
  facet_wrap(. ~ month)

# LEAN FACTOR
dat %>%
  filter(date >= ymd(20200301)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(state, month) %>%
  summarize(cases = sum(new_cases) / unique(pop) * 100000,
            lean = mean(lean_avg, na.rm = TRUE)) %>%
  ggplot(aes(x = lean, y = cases, label = state)) +
  geom_vline(xintercept = 0, size = .5, col = "40grey") +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(color = "dark blue", alpha = .5) +
  geom_smooth(method = "lm") +

  theme_DataStache() +
  ggtitle("Does State Political Lean Impact New Case Load") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "Lean based on FiveThirtyEight SLPLI and Cook PVI") +
  facet_wrap(. ~ month)

# POLICY STRINGENCY FACTOR
dat %>%
  filter(date >= ymd(20200301)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(state, month) %>%
  summarize(cases = sum(new_cases) / unique(pop) * 100000,
            string = mean(stringency_index, na.rm = TRUE)) %>%
  ggplot(aes(x = string, y = cases, label = state)) +
#  geom_vline(xintercept = 0, size = .5, col = "40grey") +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(color = "dark blue", alpha = .5) +
  geom_smooth(method = "lm") +

  theme_DataStache() +
  ggtitle("Does Stringency Index Impact New Case Load") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "Lean based on FiveThirtyEight SLPLI and Cook PVI") +
  facet_wrap(. ~ month)

# POLICY GOVERNMENT FACTOR
dat %>%
  filter(date >= ymd(20200301)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(state, month) %>%
  summarize(cases = sum(new_cases) / unique(pop) * 100000,
            gov = mean(government_index, na.rm = TRUE)) %>%
  ggplot(aes(x = gov, y = cases, label = state)) +
  #  geom_vline(xintercept = 0, size = .5, col = "40grey") +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(color = "dark blue", alpha = .5) +
  geom_smooth(method = "lm") +

  theme_DataStache() +
  ggtitle("Does Government Index Impact New Case Load") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "Lean based on FiveThirtyEight SLPLI and Cook PVI") +
  facet_wrap(. ~ month)

# POLICY CONTAINMENT FACTOR
dat %>%
  filter(date >= ymd(20200301)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(state, month) %>%
  summarize(cases = sum(new_cases) / unique(pop) * 100000,
            cont = mean(containment_index, na.rm = TRUE)) %>%
  ggplot(aes(x = cont, y = cases, label = state)) +
  #  geom_vline(xintercept = 0, size = .5, col = "40grey") +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(color = "dark blue", alpha = .5) +
  geom_smooth(method = "lm") +
  theme_DataStache() +
  ggtitle("Does Containment Index Impact New Case Load") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "Lean based on FiveThirtyEight SLPLI and Cook PVI") +
  facet_wrap(. ~ month)

# POLICY ECONOMIC FACTOR (not much here)
dat %>%
  filter(date >= ymd(20200301)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(state, month) %>%
  summarize(cases = sum(new_cases) / unique(pop) * 100000,
            econ = mean(economic_index, na.rm = TRUE)) %>%
  ggplot(aes(x = econ, y = cases, label = state)) +
  #  geom_vline(xintercept = 0, size = .5, col = "40grey") +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(color = "dark blue", alpha = .5) +
  geom_smooth(method = "lm") +
  theme_DataStache() +
  ggtitle("Does Economic Index Impact New Case Load") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "Lean based on FiveThirtyEight SLPLI and Cook PVI") +
  facet_wrap(. ~ month)

# POLICY MEAN INDEX - TOP 3 FACTOR
dat %>%
  filter(date >= ymd(20200301)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(state, month) %>%
  summarize(cases = sum(new_cases) / unique(pop) * 100000,
            mean = mean(mean_3, na.rm = TRUE)) %>%
  ggplot(aes(x = mean, y = cases, label = state)) +
  #  geom_vline(xintercept = 0, size = .5, col = "40grey") +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(color = "dark blue", alpha = .5) +
  geom_smooth(method = "lm") +
  theme_DataStache() +
  ggtitle("Does Mean Top 3 Indexs Impact New Case Load") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "Lean based on FiveThirtyEight SLPLI and Cook PVI") +
  facet_wrap(. ~ month)


# PERCENT POSITIV
dat %>%
  filter(date >= ymd(20200301) & date <= ymd(20201130)) %>%
  mutate(month = month(date, label = TRUE),
         positive = new_cases / new_tests) %>%
  group_by(state, month) %>%
  summarize(cases = sum(new_cases) / unique(pop) * 100000,
            positives = mean(positive, na.rm = TRUE)) %>%
  ggplot(aes(x = positives, y = cases, label = state)) +
  #  geom_vline(xintercept = 0, size = .5, col = "40grey") +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(color = "dark blue", alpha = .5) +
  geom_smooth(method = "lm") +
  theme_DataStache() +
  ggtitle("Does Mean Top 3 Indexs Impact New Case Load") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "Lean based on FiveThirtyEight SLPLI and Cook PVI") +
  facet_wrap(. ~ month)


##### BUILD DATA SET OF JUST RELEVANT VARIABLES #####
head(dat)

model <- dat %>%
  select(state, state_name, date, pop, new_cases, new_tests, new_death, hosp,
         stringency_index, containment_index, government_index, lean_avg, tavg, r_mean, workplace, residential) %>%
  filter(!new_cases == 0 & !new_tests == 0 & !state == 'PR')
save(model, file = 'rda/model.rda')
