library(caret)
library(tidyverse)
library(zoo)
library(lubridate)
library(tidylog)

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

# FILTER NAs
model <- model %>%
  filter(!is.na(r_mean) & !is.na(retail_recreation) & !is.na(workplace) & !is.na(residential) & !is.na(tavg))

# MAKE MODEL DATA PER 100k 
head(model)
model <- model %>%
  mutate(new_cases = new_cases / pop * 100000,
         new_tests = new_tests / pop * 100000,
         new_death = new_death / pop * 100000,
         hosp = hosp / pop * 100000)

# STRATIFY TEMPERATURES
head(model)
model <- model %>%
  mutate(tavg = round(tavg),
         tavg_strata = case_when(tavg < 0 ~ 'Sub 0',
                                 tavg %in% 1:5 ~ '0-5',
                                 tavg %in% 6:10 ~ '6-10',
                                 tavg %in% 11:15 ~ '11-15',
                                 tavg %in% 16:20 ~ '16-20',
                                 tavg %in% 21:25 ~ '21-25',
                                 tavg %in% 26:30 ~ '26-30',
                                 tavg %in% 31:35 ~ '31-35',
                                 tavg %in% 36:40 ~ '36-40',
                                 tavg %in% 41:45 ~ '41-45',
                                 tavg %in% 46:50 ~ '46-50',
                                 tavg %in% 51:55 ~ '51-55',
                                 tavg %in% 56:60 ~ '56-60',
                                 tavg %in% 61:65 ~ '61-65',
                                 tavg %in% 66:70 ~ '66-70',
                                 tavg %in% 71:75 ~ '71-75',
                                 tavg %in% 76:80 ~ '76-80',
                                 tavg %in% 81:85 ~ '81-85',
                                 tavg %in% 86:90 ~ '86-90',
                                 tavg %in% 91:95 ~ '91-95',
                                 tavg %in% 96:100 ~ '96-100',
                                 tavg > 100 ~ '100+'),
         policy_index_mean = round(policy_index_mean),
         policy_strata = case_when(policy_index_mean %in% 1:5 ~ '0-5',
                                   policy_index_mean %in% 6:10 ~ '6-10',
                                   policy_index_mean %in% 11:15 ~ '11-15',
                                   policy_index_mean %in% 16:20 ~ '16-20',
                                   policy_index_mean %in% 21:25 ~ '21-25',
                                   policy_index_mean %in% 26:30 ~ '26-30',
                                   policy_index_mean %in% 31:35 ~ '31-35',
                                   policy_index_mean %in% 36:40 ~ '36-40',
                                   policy_index_mean %in% 41:45 ~ '41-45',
                                   policy_index_mean %in% 46:50 ~ '46-50',
                                   policy_index_mean %in% 51:55 ~ '51-55',
                                   policy_index_mean %in% 56:60 ~ '56-60',
                                   policy_index_mean %in% 61:65 ~ '61-65',
                                   policy_index_mean %in% 66:70 ~ '66-70',
                                   policy_index_mean %in% 71:75 ~ '71-75',
                                   policy_index_mean %in% 76:80 ~ '76-80',
                                   policy_index_mean %in% 81:85 ~ '81-85',
                                   policy_index_mean %in% 86:90 ~ '86-90',
                                   policy_index_mean %in% 91:95 ~ '91-95',
                                   policy_index_mean %in% 96:100 ~ '96-100'))
head(model)

x2_wk_shift <- model %>%
  select(-pop, -new_cases, - new_tests, -new_death, -hosp) %>%
  mutate(metric_date = date,
         date = date+14)

model <- model %>%
  select(state, date, pop, new_cases, new_tests, new_death, hosp) %>%
  right_join(x2_wk_shift) %>%
  filter(date >= ymd(20200701) & !is.na(new_cases)) %>%
  ungroup()


library(caret)
set.seed(20201201, sample.kind = "Rounding")
test_index <- createDataPartition(y = model$new_cases, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- model[-test_index,]
test_set <- model[test_index,]

RMSE <- function(true_new_cases, predicted_new_cases){
  sqrt(mean((true_new_cases - predicted_new_cases)^2))
}

##### BEGIN MODEL #####
mu_hat <- mean(train_set$new_cases)
mu_hat

naive_rmse <- RMSE(test_set$new_cases, mu_hat) # COMPUTE RMSE BAED ON JUST THE AVERAGE
naive_rmse

rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse) # A TABLE WHERE WE STORE RESULTS OF OUR TRAINING
rmse_results


### MAKE IT AVERAGE BY LEAN
mu <- mean(train_set$new_cases) 
lean_avgs <- train_set %>% 
  group_by(lean_avg) %>% 
  summarize(b_l = mean(new_cases - mu)) # b_l is the average based on partisan lean

# HISTOGRAM OF b_l
lean_avgs %>% qplot(b_l, geom ="histogram", bins = 20, data = ., color = I("black"))

# MODEL b_l (lean effect)
predicted_cases <- mu + test_set %>% 
  left_join(lean_avgs, by='lean_avg') %>%
  .$b_l

model_1_rmse <- RMSE(predicted_cases, test_set$new_cases)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Partisan Lean Model",
                                     RMSE = model_1_rmse ))

rmse_results %>% knitr::kable() 


### MAKE IT AVERAGE BY TEMPERATURE STRATA
temp_avg <- train_set %>% 
  left_join(lean_avgs, by='lean_avg') %>%
  group_by(tavg_strata) %>%
  summarize(b_t = mean(new_cases - mu - b_l)) # b_t is the average based on temperatrue strata

predicted_cases <- test_set %>% 
  left_join(lean_avgs, by='lean_avg') %>%
  left_join(temp_avg, by='tavg_strata') %>%
  mutate(pred = mu + b_l + b_t) %>%
  .$pred

model_2_rmse <- RMSE(predicted_cases, test_set$new_cases)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="State Lean + Temp Effect",  
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()



### START AT REGULARIZING DATA
lambdas <- seq(0, 10, .25)
mu <- mean(train_set$new_cases)

rmses <- sapply(lambdas, function(lambda){
    temp_reg_avgs <- train_set %>% 
      left_join(lean_avgs, by="lean_avg") %>%
      group_by(tavg_strata) %>%
      summarize(b_tl = sum(new_cases - b_l - mu) / (n() + lambda))
    predicted_new_cases <- test_set %>% 
      left_join(lean_avgs, by = "lean_avg") %>%
      left_join(temp_reg_avgs, by = "tavg_strata") %>%
      mutate(pred = mu + b_l + b_tl) %>%
      .$pred
  return(RMSE(predicted_new_cases, test_set$new_cases))
})
qplot(lambdas, rmses)  
lambdas[which.min(rmses)]



### ADD POLICY MEAN STRATA
policy_avg <- train_set %>% 
  left_join(lean_avgs, by='lean_avg') %>%
  left_join(temp_avg, by='tavg_strata') %>%
  group_by(policy_strata) %>%
  summarize(b_p = mean(new_cases - mu - b_l - b_t)) # b_p is the average based on policy

predicted_cases <- test_set %>% 
  left_join(lean_avgs, by='lean_avg') %>%
  left_join(temp_avg, by='tavg_strata') %>%
  left_join(policy_avg, by='policy_strata') %>%
  mutate(pred = mu + b_l + b_t + b_p) %>%
  .$pred

model_2_rmse <- RMSE(predicted_cases, test_set$new_cases)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="State Lean + Temp Effect + Policy",  
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()

### ADD DAY OF WEEK MEAN
day_avg <- train_set %>% 
  left_join(lean_avgs, by='lean_avg') %>%
  left_join(temp_avg, by='tavg_strata') %>%
  left_join(policy_avg, by='policy_strata') %>%
  group_by(day) %>%
  summarize(b_d = mean(new_cases - mu - b_l - b_t - b_p)) # b_d is the average based on policy

predicted_cases <- test_set %>% 
  left_join(lean_avgs, by='lean_avg') %>%
  left_join(temp_avg, by='tavg_strata') %>%
  left_join(policy_avg, by='policy_strata') %>%
  left_join(day_avg, by='day') %>%
  mutate(pred = mu + b_l + b_t + b_p + b_d) %>%
  .$pred

model_2_rmse <- RMSE(predicted_cases, test_set$new_cases)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="State Lean + Temp Effect + Policy + Day",  
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()


# LARGEST ERRORS
test_set %>% 
  left_join(lean_avgs, by='lean_avg') %>%
  left_join(temp_avg, by='tavg_strata') %>%
  left_join(policy_avg, by='policy_strata') %>%
  mutate(residual = new_cases - (mu + b_l + b_t + b_p)) %>%
  arrange(desc(abs(residual))) %>% 
  select(state, date, residual) %>% slice(1:10) %>% knitr::kable()




