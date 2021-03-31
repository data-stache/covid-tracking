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

STATE <- 'NY'

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




