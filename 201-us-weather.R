library(broom)
library(tidyverse)
library(lubridate)
library(zoo)

load("rda/covid.rda")
load("rda/weather.rda")
load("rda/ind_red_cases.rda")
load("rda/theme_DataStache.rda")

options(scipen = 999)
options(digits = 4)

early_states <- c("CT", "DE", "DC", "IL", "IN", "IA", "LA", "MD", "MA", "MI", "MN", "MS", "NE", "NJ", "NY", "PA", "RI", "SD")
east_coast_early <- c("CT", "DC", "MD", "MA", "NY", "NJ")

##### USA REGRESSION ANALYSIS #####
dat <- covid %>% 
  select(date, state, state_name, new_cases_percap) %>%
  left_join(weather_usa) %>%
  select(1:5) %>%
  filter(!is.na(tavg)) %>%
  group_by(state) %>%
  mutate(cases = rollapply(new_cases_percap, width = 14, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"),
         temp = rollapply(tavg, width = 14, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"))


##### REGRESSION ANALYTICS #####
# ALL USA WHOLE PANDEMIC
dat %>%
  filter(date >= ymd(20200401)) %>%
  ggplot(aes(x = temp, y = cases)) +
  geom_point(size = 1, alpha = .5) +
  geom_smooth()

# MONTH FACET WRAP WHOLE PANDEMIC
dat %>%
  filter(date >= ymd(20200401)) %>%
  ggplot(aes(x = temp, y = cases)) +
  geom_point(size = 1, alpha = .5) +
  facet_wrap(. ~ month(date))

# STATE CORRELATIONS
cor <- dat %>%
  filter(date >= ymd(20200401)) %>%
  group_by(state) %>%
  summarise(cor = cor(cases, temp))
cor %>%
  filter(cor > .3 | cor < -.3)

# MODEL STATES
coeffs <- dat %>%
  filter(date >= ymd(20200401)) %>%
  group_by(state) %>%
  do(tidy(lm(cases ~ temp, data = .),conf.int = TRUE))
coeffs %>%
  filter(term == "temp" & p.value <= .05)




##### STATE MODELS #####
# PICK A STATE
st <- "MD"

dat_st <- dat %>%
  filter(state %in% st)

fit_st <- lm(cases ~ temp, data = dat_st)
fit_st

coeffs_st <- tidy(fit_st, conf.int = TRUE)
coeffs_st

# CASES BY NEW TEMPERATURE
new_temps <- data.frame(temp = seq(0,100,5))
new_temps <- predict(fit_st, newdata = new_temps) %>%
  cbind(new_temps) %>%
  rename(cases_mod = ".")
new_temps <- new_temps[,c(2,1)]
new_temps

# STRAIGHT LINEAR MODEL
dat_st %>%
  filter(date >= ymd(20200401)) %>%
  ggplot(aes(x = temp, y = cases)) +
  geom_hline(yintercept = 0, col = "grey60", size = .7) +
  geom_point(size = .5, alpha = .3, col = "dark blue") +
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = seq(0,100,5)) +
  scale_y_continuous(breaks = seq(0,200,5), expand = c(0,0)) +
  coord_cartesian(ylim = c(0,max(dat_st$cases, na.rm = TRUE) * 1.05)) +
  ggtitle("Does Average Temperature Affect Covid-19 Infection Rates") +
  xlab("Last 14 Day Average Temperature (°F)") +
  ylab("Next 14 Day Average Daily New Cases") +
  theme_DataStache() +
  theme(axis.title.x.bottom = element_text(size = rel(.7), margin = margin(5, 1, 1, 1)),
        axis.title.y.left = element_text(angle = 90, size = rel(.6), margin = margin(1, 7, 1, 1)))

# FIT TO POINTS MODEL
dat_st %>%
  filter(date >= ymd(20200401)) %>%
  ggplot(aes(x = temp, y = cases)) +
  geom_hline(yintercept = 0, col = "grey60", size = .7) +
  geom_point(size = .5, alpha = .3, col = "dark blue") +
  geom_smooth() +
  scale_x_continuous(breaks = seq(0,100,5)) +
  scale_y_continuous(breaks = seq(0,200,5), expand = c(0,0)) +
  coord_cartesian(ylim = c(0,max(dat_st$cases, na.rm = TRUE) * 1.05)) +
  ggtitle("Does Average Temperature Affect Covid-19 Infection Rates") +
  xlab("Last 14 Day Average Temperature (°F)") +
  ylab("Next 14 Day Average Daily New Cases") +
  theme_DataStache() +
  theme(axis.title.x.bottom = element_text(size = rel(.7), margin = margin(5, 1, 1, 1)),
        axis.title.y.left = element_text(angle = 90, size = rel(.6), margin = margin(1, 7, 1, 1)))

