library(broom)
library(tidyverse)
library(lubridate)

load("rda/covid.rda")
load("rda/weather.rda")

early_states <- c("CT", "DE", "DC", "IL", "IN", "IA", "LA", "MD", "MA", "MI", "MN", "MS", "NE", "NJ", "NY", "PA", "RI", "SD")

##### USA REGRESSION ANALYSIS #####
dat <- covid %>% 
  select(date, state, state_name, new_cases, pop) %>%
  left_join(weather_usa) %>%
  mutate(cases_pc = new_cases / pop * 100000)

##### SINCE APRIL PANDEMIC #####
st <- "CA"
dat %>%
  filter(date >= ymd(20200501) & state == st) %>%
  ggplot(aes(x = tavg_07da, y = cases_pc)) +
  geom_point(size = 1, alpha = .5) +
  geom_smooth()

dat %>%
  filter(state == st) %>%
  ggplot(aes(x = tavg_07da, y = cases_pc)) +
  geom_point(size = 1, alpha = .5) +
  facet_wrap(. ~ month(date))

model <- dat %>%
  filter(date >= ymd(20200401)) %>%
  lm(new_cases_07da ~ tavg_07da, data = .)
model

coefs <- tidy(model, conf.int = TRUE)
coefs



##### EARLY PANDEMIC STATES #####
dat %>%
  filter(date >= ymd(20200401) & state %in% early_states) %>%
  ggplot(aes(x = tavg_07da, y = cases_pc)) +
  geom_point(size = 1, alpha = .5) +
  geom_smooth()

dat %>%
  filter(date >= ymd(20200401) & state %in% early_states) %>%
  ggplot(aes(x = tavg_07da, y = cases_pc)) +
  geom_point(size = 1, alpha = .5) +
  facet_wrap(. ~ month(date))

cor <- dat %>%
  filter(date >= ymd(20200401) & state %in% early_states) %>%
  group_by(state) %>%
  summarise(cor = cor(cases_pc, tavg_07da))

state_model <- function(data) {
  model <- lm(cases_pc ~ tavg_07da, data = data)
  data.frame(slope = model$coefficients[2],
             se = summary(model)$coefficients[2,2])
}

model <- dat %>%
  filter(date >= ymd(20200401) & state %in% early_states) %>%
  group_by(state) %>%
  do(state_model(.))
model

coefs <- tidy(model, conf.int = TRUE)
coefs

model_x <- lm(cases_pc ~ tavg_07da, data = dat)
model_x


coefs_x <- tidy(model_x, conf.int = TRUE)
coefs_x


##### PREDICTED CASES PER DAY #####
new_temps <- data.frame(tavg_07da = seq(0,60,5))
new_temps <- predict(model, newdata = new_temps) %>%
  cbind(new_temps) %>%
  rename(new_cases_07da_mod = ".")
new_temps <- new_temps[,c(2,1)]
new_temps


##### PLOT PREDICTION VS ACTUAL #####
dat %>%
  filter(date >= ymd(20200401)) %>%
  ggplot(aes(x = tavg_07da, y = new_cases_07da)) +
  geom_hline(yintercept = 0, col = "grey60", size = .7) +
  geom_point(size = .5, alpha = .3, col = "dark blue") +
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = seq(0,100,5)) +
  scale_y_continuous(breaks = seq(0,100000,200), expand = c(0,0)) +
  coord_cartesian(ylim = c(0,max(dat$new_cases_07da, na.rm = TRUE) * 1.05)) +
  ggtitle("Does Average Temperature Affect Covid-19 Infection Rates") +
  xlab("7 Day Average Temperature (°F)") +
  ylab("7 Day Average Daily New Cases") +
  theme_DataStache() +
  theme(axis.title.x.bottom = element_text(size = rel(.7), margin = margin(5, 1, 1, 1)),
        axis.title.y.left = element_text(angle = 90, size = rel(.6), margin = margin(1, 7, 1, 1)))

p_width <- 9
p_height <- (9/16) * p_width
ggsave("figs/nd-weather-infection-regression.png",
       width = p_width,
       height = p_height,
       dpi = "retina")