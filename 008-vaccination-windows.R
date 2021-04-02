# Load Libraries ---------------------------------------------------------------
{
  library(lubridate)
  library(tidyverse)
  library(tidylog)
  library(ggplot2)
  load("/Users/andrewgriffin/projects/zConstants/rda/theme_DataStache.rda")
}
options(scipen = 999)

RUN_DATE <- Sys.Date()

populations <- read.csv("data/us-census-population-data.csv")
populations <- populations %>%
  mutate(state = factor(state),
         threshold = population * .7) %>%
  select(state, state_name, population, threshold)



# Load Data --------------------------------------------------------------------
load("rda/vaccinations.rda")

STATE <- 'CO'

tdy_date <- max(vaccinations$date)

dat <- vaccinations %>%
  filter(state == STATE)

COUNT <- count(dat) %>%
  .$n

THRESHOLD <- populations$threshold[populations$state == STATE]

# Exponential ------------------------------------------------------------------
# Pull LM Data
dat <- dat %>%
  select(date, people_fully_vaccinated) %>%
  arrange(date) %>%
  mutate(day_count = seq(1, COUNT, 1))

# Prediction Data Frame
day_count <- seq(1, 365*2, 1)
day_count <- as.data.frame(day_count)

# Fit and predict
fit_exp <- lm(sqrt(people_fully_vaccinated) ~ day_count, data = dat)
pred_exp <- predict(fit_exp, day_count)
pred_exp <- pred_exp^2

# Count of Days vector
days <- names(pred_exp)

# Day vaccinated pop exceeds threshold
value <- min(pred_exp[pred_exp >= THRESHOLD])
days_subset <- pred_exp == value

# Which day specifically
count <- days[days_subset]
count <- as.integer(count)

# LM Start date
start <- min(dat$date)

# Modeled Date
exp_pred_date <- start + count



# Linear -----------------------------------------------------------------------
# Fit and predict
fit_lm <- lm(people_fully_vaccinated ~ day_count, data = dat)
pred_lm <- predict(fit_lm, day_count)

# Count of Days vector
days <- names(pred_lm)

# Day vaccinated pop exceeds threshold
value <- min(pred_lm[pred_lm >= THRESHOLD])
days_subset <- pred_lm == value

# Which day specifically
count <- days[days_subset]
count <- as.integer(count)

# LM Start date
start <- min(dat$date)

# Modeled Date
lm_pred_date <- start + count



# The Window -------------------------------------------------------------------
window <- c(exp_pred_date, lm_pred_date)

window



# Graph of Fits ----------------------------------------------------------------

pred_exp <- as.integer(pred_exp)
pred_lm <- as.integer(pred_lm)

dat_visual <- data.frame(day_count,
                         pred_exp,
                         pred_lm)

G <- dat_visual %>%
  left_join(dat) %>%
  select(state, day_count, date, people_fully_vaccinated, pred_lm, pred_exp) %>%
  mutate(state = paste(STATE),
         date = seq(ymd(20210112), ymd(20210112) + (max(day_count)-1), 'day')) %>%
  ggplot(aes(x = date)) +
  ggtitle(paste(STATE, '- Potential Dates to Reach 70% Immunization')) +
  geom_ribbon(aes(ymax = pred_exp, ymin = pred_lm), color = 'grey', alpha = .2) +
  geom_hline(yintercept = THRESHOLD, size = .5, color = 'red') +
  geom_point(aes(y = people_fully_vaccinated), alpha = .5, size = .3) +
  geom_line(aes(y = pred_lm), color = 'darkblue', size = .2) +
  geom_line(aes(y = pred_exp), color = 'darkgreen', size = .2) +
  geom_label(aes(x = window[1], y = THRESHOLD, label = window[1]), size = 2) +
  geom_label(aes(x = window[2], y = THRESHOLD, label = window[2]), size = 2) +
  coord_cartesian(ylim = c(0, THRESHOLD + 1000), xlim = c(min(dat$date), lm_pred_date)) +
  theme_DataStache()

G

p_width <- 12
p_height <- (9/16) * p_width  

ggsave(paste("figs/", STATE, '-', tdy_date, ".png", sep = ""),
       G,
       width = p_width,
       height = p_height,
       dpi = "retina")
