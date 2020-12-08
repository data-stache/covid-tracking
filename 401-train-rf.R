library(caret)
library(tidyverse)
library(zoo)
library(lubridate)
library(tidylog)

options(scipen = 999)

##### BUILD MODEL SET #####
load('rda/covid.rda')
load('rda/transmission.rda')
load('rda/mobility.rda')
load('rda/weather_usa.rda')
load('rda/covid_pol.rda')
load('rda/policy.rda')
load('rda/theme_DataStache.rda')

# Model Data Set
model <- covid %>%
  select(state, state_name, date, pop, new_cases, new_tests, new_death, hosp, day)

# Transmission Data Set
model <- model %>%
  left_join(transmission)

# Add Mobility
dat_mob <- mobility %>%
  select(state_name, date, retail_recreation, workplace, residential)

model <- model %>%
  left_join(dat_mob)

# Add Weather
dat_weather <- weather_usa %>%
  select(date, state, tavg)

model <- model %>%
  left_join(dat_weather)

# Add State Lean
dat_lean <- covid_pol %>%
  select(date, state, lean_avg)

model <- model %>%
  left_join(dat_lean)

# Add State Policy
dat_pol <- policy %>%
  group_by(state, date) %>%
  mutate(policy_index_mean = (stringency_index + government_index + containment_index) / 3) %>%
  select(state, date, policy_index_mean)

model <- model %>%
  left_join(dat_pol) %>%
  select(-state_name) %>%
  filter(!state == 'PR')

save(model, file = 'rda/model.rda')

# CLEAR ENVIRONS
rm(list=ls())

load('rda/model.rda')


##### DATA MANIPULATE MODEL SET #####
head(model)

# MAKE MODEL DATA PER 100k 
head(model)
model <- model %>%
  mutate(new_cases = new_cases / pop * 100000,
         new_tests = new_tests / pop * 100000,
         new_death = new_death / pop * 100000,
         hosp = hosp / pop * 100000)

# SHIFT INDICATORS BY TWO WEEKS
x2_wk_shift <- model %>%
  select(-pop, -new_cases, - new_tests, -new_death, -hosp) %>%
  mutate(metric_date = date,
         date = date+14)

# MERGE TO MODEL
model <- model %>%
  select(date, new_cases, hosp) %>%
  left_join(model) %>%
  filter(date >= ymd(20200701) & !is.na(new_cases)) %>%
  ungroup()

# REMOVE NAS
model <- na.omit(model)

names(model)

model <- model %>%
  select(new_cases, day, tavg, policy_index_mean, lean_avg, r_mean, retail_recreation, workplace, residential)

library(caret)
set.seed(20201201, sample.kind = "Rounding")
test_index <- createDataPartition(y = model$new_cases, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- model[-test_index,]
test_set <- model[test_index,]

#fit <- train(new_cases ~ ., 
#             method = "rf",
#             data = train_set,
#             nodesize = 1,
#             tuneGrid = data.frame(mtry = seq(1, 11, 1)))
ggplot(fit, highlight = TRUE)

varImp(fit)


##### ENSEMBLE MODEL #####
models <- c("glm", "svmLinear", "knn", "gamLoess", "rf")

fits <- lapply(models, function(model){ 
  print(model)
  train(new_cases ~ ., method = model, data = train_set)
}) 

names(fits) <- models

pred <- sapply(fits, function(object) 
  predict(object, newdata = test_set))

i <- seq(1,5)
pred_models <- sapply(i, function(i){
  mean(pred[,i] == test_set$new_cases)
})





















